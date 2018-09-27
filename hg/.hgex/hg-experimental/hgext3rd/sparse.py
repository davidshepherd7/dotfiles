# sparse.py - allow sparse checkouts of the working directory
#
# Copyright 2014 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

"""allow sparse checkouts of the working directory
"""

from mercurial import util, cmdutil, extensions, context, dirstate, commands
from mercurial import localrepo, error, hg, pathutil, registrar, patch
from mercurial import match as matchmod
from mercurial import merge as mergemod
from mercurial.node import nullid
from mercurial.i18n import _
import os, collections, hashlib

cmdtable = {}
command = registrar.command(cmdtable)
testedwith = 'ships-with-fb-hgext'

cwdrealtivepatkinds = ('glob', 'relpath')

def _fbsparseexists(ui):
    # internal config: extensions.fbsparse
    with ui.configoverride({("devel", "all-warnings"): False}):
        return not ui.config('extensions', 'fbsparse', '!').startswith('!')

def uisetup(ui):
    if _fbsparseexists(ui):
        cmdtable.clear()
        return
    _setupupdates(ui)
    _setupcommit(ui)

def extsetup(ui):
    if _fbsparseexists(ui):
        cmdtable.clear()
        return
    _setupclone(ui)
    _setuplog(ui)
    _setupadd(ui)
    _setupdirstate(ui)
    _setupdiff(ui)
    # if fsmonitor is enabled, tell it to use our hash function
    try:
        fsmonitor = extensions.find('fsmonitor')
        def _hashignore(orig, ignore):
            return _hashmatcher(ignore)
        extensions.wrapfunction(fsmonitor, '_hashignore', _hashignore)
    except KeyError:
        pass
    # do the same for hgwatchman, old name
    try:
        hgwatchman = extensions.find('hgwatchman')
        def _hashignore(orig, ignore):
            return _hashmatcher(ignore)
        extensions.wrapfunction(hgwatchman, '_hashignore', _hashignore)
    except KeyError:
        pass

def reposetup(ui, repo):
    if _fbsparseexists(repo.ui):
        return
    if not util.safehasattr(repo, 'dirstate'):
        return

    _wraprepo(ui, repo)

def replacefilecache(cls, propname, replacement):
    """Replace a filecache property with a new class. This allows changing the
    cache invalidation condition."""
    origcls = cls
    assert callable(replacement)
    while cls is not object:
        if propname in cls.__dict__:
            orig = cls.__dict__[propname]
            setattr(cls, propname, replacement(orig))
            break
        cls = cls.__bases__[0]

    if cls is object:
        raise AttributeError(_("type '%s' has no property '%s'") % (origcls,
                             propname))

def _setupupdates(ui):
    def _calculateupdates(orig, repo, wctx, mctx, ancestors, branchmerge, *arg,
                          **kwargs):
        """Filter updates to only lay out files that match the sparse rules.
        """
        actions, diverge, renamedelete = orig(repo, wctx, mctx, ancestors,
                                              branchmerge, *arg, **kwargs)

        if not util.safehasattr(repo, 'sparsematch'):
            return actions, diverge, renamedelete

        files = set()
        prunedactions = {}
        oldrevs = [pctx.rev() for pctx in wctx.parents()]
        oldsparsematch = repo.sparsematch(*oldrevs)

        if branchmerge:
            # If we're merging, use the wctx filter, since we're merging into
            # the wctx.
            sparsematch = repo.sparsematch(wctx.parents()[0].rev())
        else:
            # If we're updating, use the target context's filter, since we're
            # moving to the target context.
            sparsematch = repo.sparsematch(mctx.rev())

        temporaryfiles = []
        for file, action in actions.iteritems():
            type, args, msg = action
            files.add(file)
            if sparsematch(file):
                prunedactions[file] = action
            elif type == 'm':
                temporaryfiles.append(file)
                prunedactions[file] = action
            elif branchmerge:
                if type != 'k':
                    temporaryfiles.append(file)
                    prunedactions[file] = action
            elif type == 'f':
                prunedactions[file] = action
            elif file in wctx:
                prunedactions[file] = ('r', args, msg)

        if len(temporaryfiles) > 0:
            ui.status(_("temporarily included %d file(s) in the sparse checkout"
                " for merging\n") % len(temporaryfiles))
            repo.addtemporaryincludes(temporaryfiles)

            # Add the new files to the working copy so they can be merged, etc
            actions = []
            message = 'temporarily adding to sparse checkout'
            wctxmanifest = repo[None].manifest()
            for file in temporaryfiles:
                if file in wctxmanifest:
                    fctx = repo[None][file]
                    actions.append((file, (fctx.flags(), False), message))

            typeactions = collections.defaultdict(list)
            typeactions['g'] = actions
            mergemod.applyupdates(repo, typeactions, repo[None], repo['.'],
                                  False)

            dirstate = repo.dirstate
            for file, flags, msg in actions:
                dirstate.normal(file)

        profiles = repo.getactiveprofiles()
        changedprofiles = profiles & files
        # If an active profile changed during the update, refresh the checkout.
        # Don't do this during a branch merge, since all incoming changes should
        # have been handled by the temporary includes above.
        if changedprofiles and not branchmerge:
            mf = mctx.manifest()
            for file in mf:
                old = oldsparsematch(file)
                new = sparsematch(file)
                if not old and new:
                    flags = mf.flags(file)
                    prunedactions[file] = ('g', (flags, False), '')
                elif old and not new:
                    prunedactions[file] = ('r', [], '')

        return prunedactions, diverge, renamedelete

    extensions.wrapfunction(mergemod, 'calculateupdates', _calculateupdates)

    def _update(orig, repo, node, branchmerge, *args, **kwargs):
        results = orig(repo, node, branchmerge, *args, **kwargs)

        # If we're updating to a location, clean up any stale temporary includes
        # (ex: this happens during hg rebase --abort).
        if not branchmerge and util.safehasattr(repo, 'sparsematch'):
            repo.prunetemporaryincludes()
        return results

    extensions.wrapfunction(mergemod, 'update', _update)

def _setupcommit(ui):
    def _refreshoncommit(orig, self, node):
        """Refresh the checkout when commits touch .hgsparse
        """
        orig(self, node)
        repo = self._repo
        if util.safehasattr(repo, 'getsparsepatterns'):
            ctx = repo[node]
            _, _, profiles = repo.getsparsepatterns(ctx.rev())
            if set(profiles) & set(ctx.files()):
                origstatus = repo.status()
                origsparsematch = repo.sparsematch()
                _refresh(repo.ui, repo, origstatus, origsparsematch, True)

            repo.prunetemporaryincludes()

    extensions.wrapfunction(context.committablectx, 'markcommitted',
        _refreshoncommit)

def _setuplog(ui):
    entry = commands.table['^log|history']
    entry[1].append(('', 'sparse', None,
        "limit to changesets affecting the sparse checkout"))

    def _logrevs(orig, repo, opts):
        revs = orig(repo, opts)
        if opts.get('sparse'):
            sparsematch = repo.sparsematch()
            def ctxmatch(rev):
                ctx = repo[rev]
                return any(f for f in ctx.files() if sparsematch(f))
            revs = revs.filter(ctxmatch)
        return revs
    extensions.wrapfunction(cmdutil, '_logrevs', _logrevs)

def _clonesparsecmd(orig, ui, repo, *args, **opts):
    include_pat = opts.get('include')
    exclude_pat = opts.get('exclude')
    enableprofile_pat = opts.get('enable_profile')
    include = exclude = enableprofile = False
    if include_pat:
        pat = include_pat
        include = True
    if exclude_pat:
        pat = exclude_pat
        exclude = True
    if enableprofile_pat:
        pat = enableprofile_pat
        enableprofile = True
    if sum([include, exclude, enableprofile]) > 1:
        raise error.Abort(_("too many flags specified."))
    if include or exclude or enableprofile:
        def clone_sparse(orig, self, node, overwrite, *args, **kwargs):
            # sparse clone is a special snowflake as in that case always
            # are outside of the repo's dir hierachy, yet we always want
            # to name our includes/excludes/enables using repo-root
            # relative paths
            overrides = {
                ('sparse', 'includereporootpaths'): True,
                ('sparse', 'enablereporootpaths'): True,
            }
            with self.ui.configoverride(overrides, 'sparse'):
                _config(self.ui, self.unfiltered(), pat, {}, include=include,
                        exclude=exclude, enableprofile=enableprofile)
            return orig(self, node, overwrite, *args, **kwargs)
        extensions.wrapfunction(hg, 'updaterepo', clone_sparse)
    return orig(ui, repo, *args, **opts)

def _setupclone(ui):
    entry = commands.table['^clone']
    entry[1].append(('', 'enable-profile', [],
                    'enable a sparse profile'))
    entry[1].append(('', 'include', [],
                    'include sparse pattern'))
    entry[1].append(('', 'exclude', [],
                    'exclude sparse pattern'))
    extensions.wrapcommand(commands.table, 'clone', _clonesparsecmd)

def _setupadd(ui):
    entry = commands.table['^add']
    entry[1].append(('s', 'sparse', None,
                    'also include directories of added files in sparse config'))

    def _add(orig, ui, repo, *pats, **opts):
        if opts.get('sparse'):
            dirs = set()
            for pat in pats:
                dirname, basename = util.split(pat)
                dirs.add(dirname)
            _config(ui, repo, list(dirs), opts, include=True)
        return orig(ui, repo, *pats, **opts)

    extensions.wrapcommand(commands.table, 'add', _add)

def _setupdirstate(ui):
    """Modify the dirstate to prevent stat'ing excluded files,
    and to prevent modifications to files outside the checkout.
    """

    def _dirstate(orig, repo):
        dirstate = orig(repo)
        dirstate.repo = repo
        return dirstate
    extensions.wrapfunction(
        localrepo.localrepository.dirstate, 'func', _dirstate)

    # The atrocity below is needed to wrap dirstate._ignore. It is a cached
    # property, which means normal function wrapping doesn't work.
    class ignorewrapper(object):
        def __init__(self, orig):
            self.orig = orig
            self.origignore = None
            self.func = None
            self.sparsematch = None

        def __get__(self, obj, type=None):
            repo = obj.repo
            origignore = self.orig.__get__(obj)
            if not util.safehasattr(repo, 'sparsematch'):
                return origignore

            sparsematch = repo.sparsematch()
            if self.sparsematch != sparsematch or self.origignore != origignore:
                self.func = unionmatcher([origignore,
                                          negatematcher(sparsematch)])
                self.sparsematch = sparsematch
                self.origignore = origignore
            return self.func

        def __set__(self, obj, value):
            return self.orig.__set__(obj, value)

        def __delete__(self, obj):
            return self.orig.__delete__(obj)

    replacefilecache(dirstate.dirstate, '_ignore', ignorewrapper)

    # dirstate.rebuild should not add non-matching files
    def _rebuild(orig, self, parent, allfiles, changedfiles=None):
        if util.safehasattr(self.repo, 'sparsematch'):
            matcher = self.repo.sparsematch()
            allfiles = allfiles.matches(matcher)
            if changedfiles:
                changedfiles = [f for f in changedfiles if matcher(f)]

            if changedfiles is not None:
                # In _rebuild, these files will be deleted from the dirstate
                # when they are not found to be in allfiles
                dirstatefilestoremove = set(f for f in self if not matcher(f))
                changedfiles = dirstatefilestoremove.union(changedfiles)

        return orig(self, parent, allfiles, changedfiles)
    extensions.wrapfunction(dirstate.dirstate, 'rebuild', _rebuild)

    # Prevent adding files that are outside the sparse checkout
    editfuncs = ['normal', 'add', 'normallookup', 'copy', 'remove', 'merge']
    hint = _('include file with `hg sparse --include <pattern>` or use ' +
             '`hg add -s <file>` to include file directory while adding')
    for func in editfuncs:
        def _wrapper(orig, self, *args):
            repo = self.repo
            if util.safehasattr(repo, 'sparsematch'):
                dirstate = repo.dirstate
                sparsematch = repo.sparsematch()
                for f in args:
                    if (f is not None and not sparsematch(f) and
                        f not in dirstate):
                        raise error.Abort(_("cannot add '%s' - it is outside "
                                            "the sparse checkout") % f,
                                          hint=hint)
            return orig(self, *args)
        extensions.wrapfunction(dirstate.dirstate, func, _wrapper)

def _setupdiff(ui):
    entry = commands.table['^diff']
    entry[1].append(('s', 'sparse', None,
                     'only show changes in files in the sparse config'))
    # wrap workingfilectx's data function to return the data for files
    # outside the sparse checkout by fetching from the working copy parent.
    def workingfilectxdata(orig, self):
        if not util.safehasattr(self.repo(), 'sparsematch'):
            return orig(self)
        sparsematch = self.repo().sparsematch()
        if sparsematch(self._path):
            return orig(self)
        else:
            basectx = self._changectx._parents[0]
            return basectx[self._path].data()
    extensions.wrapfunction(context.workingfilectx, 'data', workingfilectxdata)

    # wrap trydiff to filter diffs if '--sparse' is set
    def trydiff(orig, repo, revs, ctx1, ctx2, modified, added, removed,
                copy, getfilectx, opts, losedatafn, prefix, relroot):
        sparsematch = repo.sparsematch()
        modified = filter(sparsematch, modified)
        added = filter(sparsematch, added)
        removed = filter(sparsematch, removed)
        copy = dict((d, s) for d, s in copy.items() if sparsematch(s))
        return orig(repo, revs, ctx1, ctx2, modified, added, removed,
                    copy, getfilectx, opts, losedatafn, prefix, relroot)
    def diff(orig, ui, repo, *pats, **opts):
        issparse = bool(opts.get('sparse'))
        if issparse:
            extensions.wrapfunction(patch, 'trydiff', trydiff)
        try:
            orig(ui, repo, *pats, **opts)
        finally:
            if issparse:
                extensions.unwrapfunction(patch, 'trydiff', trydiff)
    extensions.wrapcommand(commands.table, 'diff', diff)

def _wraprepo(ui, repo):
    class SparseRepo(repo.__class__):
        def readsparseconfig(self, raw):
            """Takes a string sparse config and returns the includes,
            excludes, and profiles it specified.
            """
            includes = set()
            excludes = set()
            current = includes
            profiles = []
            for line in raw.split('\n'):
                line = line.strip()
                if not line or line.startswith('#'):
                    # empty or comment line, skip
                    continue
                elif line.startswith('%include '):
                    line = line[9:].strip()
                    if line:
                        profiles.append(line)
                elif line == '[include]':
                    if current != includes:
                        raise error.Abort(_('.hg/sparse cannot have includes ' +
                            'after excludes'))
                    continue
                elif line == '[exclude]':
                    current = excludes
                elif line:
                    if line.strip().startswith('/'):
                        self.ui.warn(_('warning: sparse profile cannot use' +
                                       ' paths starting with /, ignoring %s\n')
                                       % line)
                        continue
                    current.add(line)

            return includes, excludes, profiles

        def getsparsepatterns(self, rev):
            """Returns the include/exclude patterns specified by the
            given rev.
            """
            if not self.vfs.exists('sparse'):
                return set(), set(), []
            if rev is None:
                raise error.Abort(_("cannot parse sparse patterns from " +
                    "working copy"))

            raw = self.vfs.read('sparse')
            includes, excludes, profiles = self.readsparseconfig(raw)

            ctx = self[rev]
            if profiles:
                visited = set()
                while profiles:
                    profile = profiles.pop()
                    if profile in visited:
                        continue
                    visited.add(profile)

                    try:
                        raw = self.getrawprofile(profile, rev)
                    except error.ManifestLookupError:
                        msg = (
                            "warning: sparse profile '%s' not found "
                            "in rev %s - ignoring it\n" % (profile, ctx))
                        if self.ui.configbool('sparse', 'missingwarning'):
                            self.ui.warn(msg)
                        else:
                            self.ui.debug(msg)
                        continue
                    pincludes, pexcludes, subprofs = \
                        self.readsparseconfig(raw)
                    includes.update(pincludes)
                    excludes.update(pexcludes)
                    for subprofile in subprofs:
                        profiles.append(subprofile)

                profiles = visited

            if includes:
                includes.add('.hg*')
            return includes, excludes, profiles

        def getrawprofile(self, profile, changeid):
            try:
                simplecache = extensions.find('simplecache')
                node = self[changeid].hex()
                def func():
                    return self.filectx(profile, changeid=changeid).data()
                key = 'sparseprofile:%s:%s' % (profile.replace('/', '__'), node)
                return simplecache.memoize(func, key,
                        simplecache.stringserializer, self.ui)
            except KeyError:
                return self.filectx(profile, changeid=changeid).data()

        def sparsechecksum(self, filepath):
            fh = open(filepath)
            return hashlib.sha1(fh.read()).hexdigest()

        def _sparsesignature(self, includetemp=True):
            """Returns the signature string representing the contents of the
            current project sparse configuration. This can be used to cache the
            sparse matcher for a given set of revs."""
            signaturecache = self.signaturecache
            signature = signaturecache.get('signature')
            if includetemp:
                tempsignature = signaturecache.get('tempsignature')
            else:
                tempsignature = 0

            if signature is None or (includetemp and tempsignature is None):
                signature = 0
                try:
                    sparsepath = self.vfs.join('sparse')
                    signature = self.sparsechecksum(sparsepath)
                except (OSError, IOError):
                    pass
                signaturecache['signature'] = signature

                tempsignature = 0
                if includetemp:
                    try:
                        tempsparsepath = self.vfs.join('tempsparse')
                        tempsignature = self.sparsechecksum(tempsparsepath)
                    except (OSError, IOError):
                        pass
                    signaturecache['tempsignature'] = tempsignature
            return '%s %s' % (str(signature), str(tempsignature))

        def invalidatecaches(self):
            self.invalidatesignaturecache()
            return super(SparseRepo, self).invalidatecaches()

        def invalidatesignaturecache(self):
            self.signaturecache.clear()

        def sparsematch(self, *revs, **kwargs):
            """Returns the sparse match function for the given revs.

            If multiple revs are specified, the match function is the union
            of all the revs.

            `includetemp` is used to indicate if the temporarily included file
            should be part of the matcher.
            """
            if not revs or revs == (None,):
                revs = [self.changelog.rev(node) for node in
                    self.dirstate.parents() if node != nullid]

            includetemp = kwargs.get('includetemp', True)
            signature = self._sparsesignature(includetemp=includetemp)

            key = '%s %s' % (str(signature), ' '.join([str(r) for r in revs]))

            result = self.sparsecache.get(key, None)
            if result:
                return result

            matchers = []
            for rev in revs:
                try:
                    includes, excludes, profiles = self.getsparsepatterns(rev)

                    if includes or excludes:
                        # Explicitly include subdirectories of includes so
                        # status will walk them down to the actual include.
                        subdirs = set()
                        for include in includes:
                            dirname = os.path.dirname(include)
                            # basename is used to avoid issues with absolute
                            # paths (which on Windows can include the drive).
                            while os.path.basename(dirname):
                                subdirs.add(dirname)
                                dirname = os.path.dirname(dirname)

                        matcher = matchmod.match(self.root, '', [],
                            include=includes, exclude=excludes,
                            default='relpath')
                        if subdirs:
                            matcher = forceincludematcher(matcher, subdirs)
                        matchers.append(matcher)
                except IOError:
                    pass

            result = None
            if not matchers:
                result = matchmod.always(self.root, '')
            elif len(matchers) == 1:
                result = matchers[0]
            else:
                result = unionmatcher(matchers)

            if kwargs.get('includetemp', True):
                tempincludes = self.gettemporaryincludes()
                result = forceincludematcher(result, tempincludes)

            self.sparsecache[key] = result

            return result

        def getactiveprofiles(self):
            revs = [self.changelog.rev(node) for node in
                    self.dirstate.parents() if node != nullid]

            activeprofiles = set()
            for rev in revs:
                _, _, profiles = self.getsparsepatterns(rev)
                activeprofiles.update(profiles)

            return activeprofiles

        def writesparseconfig(self, include, exclude, profiles):
            raw = '%s[include]\n%s\n[exclude]\n%s\n' % (
                ''.join(['%%include %s\n' % p for p in sorted(profiles)]),
                '\n'.join(sorted(include)),
                '\n'.join(sorted(exclude)))
            self.vfs.write("sparse", raw)
            self.invalidatesignaturecache()

        def addtemporaryincludes(self, files):
            includes = self.gettemporaryincludes()
            for file in files:
                includes.add(file)
            self._writetemporaryincludes(includes)

        def gettemporaryincludes(self):
            existingtemp = set()
            if self.vfs.exists('tempsparse'):
                raw = self.vfs.read('tempsparse')
                existingtemp.update(raw.split('\n'))
            return existingtemp

        def _writetemporaryincludes(self, includes):
            raw = '\n'.join(sorted(includes))
            self.vfs.write('tempsparse', raw)
            self.invalidatesignaturecache()

        def prunetemporaryincludes(self):
            if repo.vfs.exists('tempsparse'):
                origstatus = self.status()
                modified, added, removed, deleted, a, b, c = origstatus
                if modified or added or removed or deleted:
                    # Still have pending changes. Don't bother trying to prune.
                    return

                sparsematch = self.sparsematch(includetemp=False)
                dirstate = self.dirstate
                actions = []
                dropped = []
                tempincludes = self.gettemporaryincludes()
                for file in tempincludes:
                    if file in dirstate and not sparsematch(file):
                        message = 'dropping temporarily included sparse files'
                        actions.append((file, None, message))
                        dropped.append(file)

                typeactions = collections.defaultdict(list)
                typeactions['r'] = actions
                mergemod.applyupdates(self, typeactions, self[None], self['.'],
                                      False)

                # Fix dirstate
                for file in dropped:
                    dirstate.drop(file)

                self.vfs.unlink('tempsparse')
                self.invalidatesignaturecache()
                msg = _("cleaned up %d temporarily added file(s) from the "
                        "sparse checkout\n")
                ui.status(msg % len(tempincludes))

    if 'dirstate' in repo._filecache:
        repo.dirstate.repo = repo
    repo.sparsecache = {}
    repo.signaturecache = {}
    repo.__class__ = SparseRepo

@command('^sparse', [
    ('I', 'include', False, _('include files in the sparse checkout')),
    ('X', 'exclude', False, _('exclude files in the sparse checkout')),
    ('d', 'delete', False, _('delete an include/exclude rule')),
    ('f', 'force', False, _('allow changing rules even with pending changes')),
    ('', 'enable-profile', False, _('enables the specified profile')),
    ('', 'disable-profile', False, _('disables the specified profile')),
    ('', 'import-rules', False, _('imports rules from a file')),
    ('', 'clear-rules', False, _('clears local include/exclude rules')),
    ('', 'refresh', False, _('updates the working after sparseness changes')),
    ('', 'reset', False, _('makes the repo full again')),
    ('', 'cwd-list', False, _('list the full contents of the current '
                              'directory')),
    ] + commands.templateopts,
    _('[--OPTION] PATTERN...'))
def sparse(ui, repo, *pats, **opts):
    """make the current checkout sparse, or edit the existing checkout

    The sparse command is used to make the current checkout sparse.
    This means files that don't meet the sparse condition will not be
    written to disk, or show up in any working copy operations. It does
    not affect files in history in any way.

    Passing no arguments prints the currently applied sparse rules.

    --include and --exclude are used to add and remove files from the sparse
    checkout. The effects of adding an include or exclude rule are applied
    immediately. If applying the new rule would cause a file with pending
    changes to be added or removed, the command will fail. Pass --force to
    force a rule change even with pending changes (the changes on disk will
    be preserved).

    --delete removes an existing include/exclude rule. The effects are
    immediate.

    --refresh refreshes the files on disk based on the sparse rules. This is
    only necessary if .hg/sparse was changed by hand.

    --enable-profile and --disable-profile accept a path to a .hgsparse file.
    This allows defining sparse checkouts and tracking them inside the
    repository. This is useful for defining commonly used sparse checkouts for
    many people to use. As the profile definition changes over time, the sparse
    checkout will automatically be updated appropriately, depending on which
    changeset is checked out. Changes to .hgsparse are not applied until they
    have been committed.

    --import-rules accepts a path to a file containing rules in the .hgsparse
    format, allowing you to add --include, --exclude and --enable-profile rules
    in bulk. Like the --include, --exclude and --enable-profile switches, the
    changes are applied immediately.

    --clear-rules removes all local include and exclude rules, while leaving
    any enabled profiles in place.

    --cwd-list list all the contents of the current directory. The files that
    are excluded by the current sparse checkout are annotated with a hyphen
    ('-') before the name.

    The following config option defines whether sparse treats supplied
    paths as relative to repo root or to the current working dir for
    include and exclude options:

        [sparse]
        includereporootpaths = off

    The following config option defines whether sparse treats supplied
    paths as relative to repo root or to the current working dir for
    enableprofile and disableprofile options:

        [sparse]
        enablereporootpaths = on

    Returns 0 if editing the sparse checkout succeeds.
    """
    include = opts.get('include')
    exclude = opts.get('exclude')
    force = opts.get('force')
    enableprofile = opts.get('enable_profile')
    disableprofile = opts.get('disable_profile')
    importrules = opts.get('import_rules')
    clearrules = opts.get('clear_rules')
    delete = opts.get('delete')
    refresh = opts.get('refresh')
    reset = opts.get('reset')
    cwdlist = opts.get('cwd_list')
    count = sum([include, exclude, enableprofile, disableprofile, delete,
                 importrules, refresh, clearrules, reset, cwdlist])
    if count > 1:
        raise error.Abort(_("too many flags specified"))

    if count == 0:
        if repo.vfs.exists('sparse'):
            ui.status(repo.vfs.read("sparse") + "\n")
            temporaryincludes = repo.gettemporaryincludes()
            if temporaryincludes:
                ui.status(_("Temporarily Included Files (for merge/rebase):\n"))
                ui.status(("\n".join(temporaryincludes) + "\n"))
        else:
            ui.status(_('repo is not sparse\n'))
        return

    if include or exclude or delete or reset or enableprofile or disableprofile:
        _config(ui, repo, pats, opts, include=include, exclude=exclude,
                reset=reset, delete=delete, enableprofile=enableprofile,
                disableprofile=disableprofile, force=force)

    if importrules:
        _import(ui, repo, pats, opts, force=force)

    if clearrules:
        _clear(ui, repo, pats, force=force)

    if refresh:
        try:
            wlock = repo.wlock()
            fcounts = map(
                len,
                _refresh(ui, repo, repo.status(), repo.sparsematch(), force))
            _verbose_output(ui, opts, 0, 0, 0, *fcounts)
        finally:
            wlock.release()

    if cwdlist:
        _cwdlist(repo)

def _config(ui, repo, pats, opts, include=False, exclude=False, reset=False,
            delete=False, enableprofile=False, disableprofile=False,
            force=False):
    """
    Perform a sparse config update. Only one of the kwargs may be specified.
    """
    wlock = repo.wlock()
    try:
        oldsparsematch = repo.sparsematch()

        if repo.vfs.exists('sparse'):
            raw = repo.vfs.read('sparse')
            oldinclude, oldexclude, oldprofiles = map(
                set, repo.readsparseconfig(raw))
        else:
            oldinclude = set()
            oldexclude = set()
            oldprofiles = set()

        try:
            if reset:
                newinclude = set()
                newexclude = set()
                newprofiles = set()
            else:
                newinclude = set(oldinclude)
                newexclude = set(oldexclude)
                newprofiles = set(oldprofiles)

            if any(os.path.isabs(pat) for pat in pats):
                err = _('paths cannot be absolute')
                raise error.Abort(err)

            adjustpats = ((include or exclude or delete) and
                    not ui.configbool('sparse', 'includereporootpaths', False))
            adjustpats |= ((enableprofile or disableprofile) and
                    not ui.configbool('sparse', 'enablereporootpaths', True))
            if adjustpats:
                # supplied file patterns should be treated as relative
                # to current working dir, so we need to convert them first
                root, cwd = repo.root, repo.getcwd()
                abspats = []
                for kindpat in pats:
                    kind, pat = matchmod._patsplit(kindpat, None)
                    if kind in cwdrealtivepatkinds or kind is None:
                        kindpat = ((kind + ':' if kind else '') +
                                pathutil.canonpath(root, cwd, pat))
                    abspats.append(kindpat)
                pats = abspats

            oldstatus = repo.status()
            if include:
                newinclude.update(pats)
            elif exclude:
                newexclude.update(pats)
            elif enableprofile:
                newprofiles.update(pats)
            elif disableprofile:
                newprofiles.difference_update(pats)
            elif delete:
                newinclude.difference_update(pats)
                newexclude.difference_update(pats)

            repo.writesparseconfig(newinclude, newexclude, newprofiles)
            fcounts = map(
                len, _refresh(ui, repo, oldstatus, oldsparsematch, force))

            profilecount = (len(newprofiles - oldprofiles) -
                            len(oldprofiles - newprofiles))
            includecount = (len(newinclude - oldinclude) -
                            len(oldinclude - newinclude))
            excludecount = (len(newexclude - oldexclude) -
                            len(oldexclude - newexclude))
            _verbose_output(
                ui, opts, profilecount, includecount, excludecount, *fcounts)
        except Exception:
            repo.writesparseconfig(oldinclude, oldexclude, oldprofiles)
            raise
    finally:
        wlock.release()

def _import(ui, repo, files, opts, force=False):
    with repo.wlock():
        # load union of current active profile
        revs = [repo.changelog.rev(node) for node in
                repo.dirstate.parents() if node != nullid]

        # read current configuration
        raw = ''
        if repo.vfs.exists('sparse'):
            raw = repo.vfs.read('sparse')
        oincludes, oexcludes, oprofiles = repo.readsparseconfig(raw)
        includes, excludes, profiles = map(
                set, (oincludes, oexcludes, oprofiles))

        # all active rules
        aincludes, aexcludes, aprofiles = set(), set(), set()
        for rev in revs:
            rincludes, rexcludes, rprofiles = repo.getsparsepatterns(rev)
            aincludes.update(rincludes)
            aexcludes.update(rexcludes)
            aprofiles.update(rprofiles)

        # import rules on top; only take in rules that are not yet
        # part of the active rules.
        changed = False
        for file in files:
            with util.posixfile(util.expandpath(file)) as importfile:
                iincludes, iexcludes, iprofiles = repo.readsparseconfig(
                    importfile.read())
                oldsize = len(includes) + len(excludes) + len(profiles)
                includes.update(iincludes - aincludes)
                excludes.update(iexcludes - aexcludes)
                profiles.update(set(iprofiles) - aprofiles)
                if len(includes) + len(excludes) + len(profiles) > oldsize:
                    changed = True

        profilecount = includecount = excludecount = 0
        fcounts = (0, 0, 0)

        if changed:
            profilecount = len(profiles - aprofiles)
            includecount = len(includes - aincludes)
            excludecount = len(excludes - aexcludes)

            oldstatus = repo.status()
            oldsparsematch = repo.sparsematch()
            repo.writesparseconfig(includes, excludes, profiles)

            try:
                fcounts = map(
                    len, _refresh(ui, repo, oldstatus, oldsparsematch, force))
            except Exception:
                repo.writesparseconfig(oincludes, oexcludes, oprofiles)
                raise

        _verbose_output(ui, opts, profilecount, includecount, excludecount,
                        *fcounts)

def _clear(ui, repo, files, force=False):
    with repo.wlock():
        raw = ''
        if repo.vfs.exists('sparse'):
            raw = repo.vfs.read('sparse')
        includes, excludes, profiles = repo.readsparseconfig(raw)

        if includes or excludes:
            oldstatus = repo.status()
            oldsparsematch = repo.sparsematch()
            repo.writesparseconfig(set(), set(), profiles)
            _refresh(ui, repo, oldstatus, oldsparsematch, force)

def _refresh(ui, repo, origstatus, origsparsematch, force):
    """Refreshes which files are on disk by comparing the old status and
    sparsematch with the new sparsematch.

    Will raise an exception if a file with pending changes is being excluded
    or included (unless force=True).
    """
    modified, added, removed, deleted, unknown, ignored, clean = origstatus

    # Verify there are no pending changes
    pending = set()
    pending.update(modified)
    pending.update(added)
    pending.update(removed)
    sparsematch = repo.sparsematch()
    abort = False
    for file in pending:
        if not sparsematch(file):
            ui.warn(_("pending changes to '%s'\n") % file)
            abort = not force
    if abort:
        raise error.Abort(_("could not update sparseness due to " +
            "pending changes"))

    # Calculate actions
    dirstate = repo.dirstate
    ctx = repo['.']
    added = []
    lookup = []
    dropped = []
    mf = ctx.manifest()
    files = set(mf)

    actions = {}

    for file in files:
        old = origsparsematch(file)
        new = sparsematch(file)
        # Add files that are newly included, or that don't exist in
        # the dirstate yet.
        if (new and not old) or (old and new and not file in dirstate):
            fl = mf.flags(file)
            if repo.wvfs.exists(file):
                actions[file] = ('e', (fl,), '')
                lookup.append(file)
            else:
                actions[file] = ('g', (fl, False), '')
                added.append(file)
        # Drop files that are newly excluded, or that still exist in
        # the dirstate.
        elif (old and not new) or (not old and not new and file in dirstate):
            dropped.append(file)
            if file not in pending:
                actions[file] = ('r', [], '')

    # Verify there are no pending changes in newly included files
    abort = False
    for file in lookup:
        ui.warn(_("pending changes to '%s'\n") % file)
        abort = not force
    if abort:
        raise error.Abort(_("cannot change sparseness due to " +
            "pending changes (delete the files or use --force " +
            "to bring them back dirty)"))

    # Check for files that were only in the dirstate.
    for file, state in dirstate.iteritems():
        if not file in files:
            old = origsparsematch(file)
            new = sparsematch(file)
            if old and not new:
                dropped.append(file)

    # Apply changes to disk
    typeactions = dict((m, [])
                       for m in 'a f g am cd dc r dm dg m e k p pr'.split())
    for f, (m, args, msg) in actions.iteritems():
        if m not in typeactions:
            typeactions[m] = []
        typeactions[m].append((f, args, msg))
    mergemod.applyupdates(repo, typeactions, repo[None], repo['.'], False)

    # Fix dirstate
    for file in added:
        dirstate.normal(file)

    for file in dropped:
        dirstate.drop(file)

    for file in lookup:
        # File exists on disk, and we're bringing it back in an unknown state.
        dirstate.normallookup(file)

    return added, dropped, lookup

def _verbose_output(ui, opts, profilecount, includecount, excludecount, added,
                    dropped, lookup):
    """Produce --verbose and templatable output

    This specifically enables -Tjson, providing machine-readable stats on how
    the sparse profile changed.

    """
    with ui.formatter('sparse', opts) as fm:
        fm.startitem()
        fm.condwrite(ui.verbose, 'profiles_added', 'Profile # change: %d\n',
                     profilecount)
        fm.condwrite(ui.verbose, 'include_rules_added',
                     'Include rule # change: %d\n', includecount)
        fm.condwrite(ui.verbose, 'exclude_rules_added',
                     'Exclude rule # change: %d\n', excludecount)
        # In 'plain' verbose mode, mergemod.applyupdates already outputs what
        # files are added or removed outside of the templating formatter
        # framework. No point in repeating ourselves in that case.
        if not fm.isplain():
            fm.condwrite(ui.verbose, 'files_added', 'Files added: %d\n',
                         added)
            fm.condwrite(ui.verbose, 'files_dropped', 'Files dropped: %d\n',
                         dropped)
            fm.condwrite(ui.verbose, 'files_conflicting',
                         'Files conflicting: %d\n', lookup)

def _cwdlist(repo):
    """ List the contents in the current directory. Annotate
    the files in the sparse profile.
    """
    ctx = repo['.']
    mf = ctx.manifest()
    cwd = util.normpath(os.getcwd())

    # Get the root of the repo so that we remove the content of
    # the root from the current working directory
    root = repo.root
    if cwd.startswith(root):
        cwd = cwd[len(root):]
    else:
        raise error.Abort(_("the current working directory should begin " +
            "with the root %s") % root)

    cwd = cwd.strip("/")
    sparsematch = repo.sparsematch(ctx.rev())
    checkedoutentries = set()
    allentries = set()
    cwdlength = len(cwd) + 1
    for filepath in mf:
        if filepath.startswith(cwd):
            tail = filepath[cwdlength:] if cwdlength > 1 else filepath
            entryname = tail.split('/', 1)[0]

            allentries.add(entryname)
            if sparsematch(filepath):
                checkedoutentries.add(entryname)

    ui = repo.ui
    for entry in sorted(allentries):
        marker = ' ' if entry in checkedoutentries else '-'
        ui.status("%s %s\n" % (marker, entry))

class forceincludematcher(object):
    """A matcher that returns true for any of the forced includes before testing
    against the actual matcher."""
    def __init__(self, matcher, includes):
        self._matcher = matcher
        self._includes = includes

    def __call__(self, value):
        return value in self._includes or self._matcher(value)

    def always(self):
        return False

    def files(self):
        return []

    def isexact(self):
        return False

    def anypats(self):
        return True

    def prefix(self):
        return False

    def visitdir(self, dir):
        if any(True for path in self._includes if path.startswith(dir)):
            return True
        return self._matcher.visitdir(dir)

    def hash(self):
        sha1 = hashlib.sha1()
        sha1.update(_hashmatcher(self._matcher))
        for include in sorted(self._includes):
            sha1.update(include + '\0')
        return sha1.hexdigest()

class unionmatcher(object):
    """A matcher that is the union of several matchers."""
    def __init__(self, matchers):
        self._matchers = matchers

    def __call__(self, value):
        for match in self._matchers:
            if match(value):
                return True
        return False

    def always(self):
        return False

    def files(self):
        return []

    def isexact(self):
        return False

    def anypats(self):
        return True

    def prefix(self):
        return False

    def visitdir(self, dir):
        for match in self._matchers:
            if match.visitdir(dir):
                return True
        return False

    def hash(self):
        sha1 = hashlib.sha1()
        for m in self._matchers:
            sha1.update(_hashmatcher(m))
        return sha1.hexdigest()

class negatematcher(object):
    def __init__(self, matcher):
        self._matcher = matcher

    def __call__(self, value):
        return not self._matcher(value)

    def always(self):
        return False

    def files(self):
        return []

    def isexact(self):
        return False

    def anypats(self):
        return True

    def visitdir(self, dir):
        return True

    def hash(self):
        sha1 = hashlib.sha1()
        sha1.update('negate')
        sha1.update(_hashmatcher(self._matcher))
        return sha1.hexdigest()

def _hashmatcher(matcher):
    if util.safehasattr(matcher, 'hash'):
        return matcher.hash()

    sha1 = hashlib.sha1()
    sha1.update(repr(matcher))
    return sha1.hexdigest()
