# __init__.py - remotefilelog extension
#
# Copyright 2013 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.
"""
The remotefilelog extension is used to leave file contents on the server and
only download them ondemand as needed.

Configs:

    ``packs.maxchainlen`` specifies the maximum delta chain length in pack files
    ``remotefilelog.backgroundprefetch`` runs prefetch in background when True
    ``remotefilelog.bgprefetchrevs`` specifies revisions to fetch on commit and
      update, and on other commands that use them. Different from pullprefetch.
    ``remotefilelog.gcrepack`` does garbage collection during repack when True
    ``remotefilelog.nodettl`` specifies maximum TTL of a node in seconds before
      it is garbage collected
    ``remotefilelog.repackonhggc`` runs repack on hg gc when True
"""

from . import fileserverclient, remotefilelog, remotefilectx, shallowstore
import shallowbundle, debugcommands, remotefilelogserver, shallowverifier
import shallowutil, shallowrepo
import repack as repackmod
from mercurial.node import hex
from mercurial.i18n import _
from mercurial.extensions import wrapfunction
from mercurial import (
    changegroup,
    changelog,
    cmdutil,
    commands,
    context,
    context,
    copies,
    debugcommands as hgdebugcommands,
    dispatch,
    error,
    exchange,
    extensions,
    hg,
    localrepo,
    match,
    merge,
    patch,
    registrar,
    repair,
    repoview,
    revset,
    scmutil,
    smartset,
    store,
    templatekw,
    util,
)

import os
import traceback

# ensures debug commands are registered
hgdebugcommands.command

try:
    from mercurial import streamclone
    streamclone._walkstreamfiles
    hasstreamclone = True
except Exception:
    hasstreamclone = False

cmdtable = {}
command = registrar.command(cmdtable)
testedwith = 'ships-with-fb-hgext'

repoclass = localrepo.localrepository
if util.safehasattr(repoclass, '_basesupported'):
    repoclass._basesupported.add(shallowrepo.requirement)
else:
    # hg <= 2.7
    repoclass.supported.add(shallowrepo.requirement)

def uisetup(ui):
    """Wraps user facing Mercurial commands to swap them out with shallow
    versions.
    """
    hg.wirepeersetupfuncs.append(fileserverclient.peersetup)

    entry = extensions.wrapcommand(commands.table, 'clone', cloneshallow)
    entry[1].append(('', 'shallow', None,
                     _("create a shallow clone which uses remote file "
                       "history")))

    extensions.wrapcommand(commands.table, 'debugindex',
        debugcommands.debugindex)
    extensions.wrapcommand(commands.table, 'debugindexdot',
        debugcommands.debugindexdot)
    extensions.wrapcommand(commands.table, 'log', log)
    extensions.wrapcommand(commands.table, 'pull', pull)

    # Prevent 'hg manifest --all'
    def _manifest(orig, ui, repo, *args, **opts):
        if shallowrepo.requirement in repo.requirements and opts.get('all'):
            raise error.Abort(_("--all is not supported in a shallow repo"))

        return orig(ui, repo, *args, **opts)
    extensions.wrapcommand(commands.table, "manifest", _manifest)

    # Wrap remotefilelog with lfs code
    def _lfsloaded(loaded=False):
        lfsmod = None
        try:
            lfsmod = extensions.find('lfs')
        except KeyError:
            pass
        if lfsmod:
            lfsmod.wrapfilelog(remotefilelog.remotefilelog)
    extensions.afterloaded('lfs', _lfsloaded)

    # debugdata needs remotefilelog.len to work
    extensions.wrapcommand(commands.table, 'debugdata', debugdatashallow)

def cloneshallow(orig, ui, repo, *args, **opts):
    if opts.get('shallow'):
        repos = []
        def pull_shallow(orig, self, *args, **kwargs):
            if shallowrepo.requirement not in self.requirements:
                repos.append(self.unfiltered())
                # set up the client hooks so the post-clone update works
                setupclient(self.ui, self.unfiltered())

                # setupclient fixed the class on the repo itself
                # but we also need to fix it on the repoview
                if isinstance(self, repoview.repoview):
                    self.__class__.__bases__ = (self.__class__.__bases__[0],
                                                self.unfiltered().__class__)
                self.requirements.add(shallowrepo.requirement)
                self._writerequirements()

                # Since setupclient hadn't been called, exchange.pull was not
                # wrapped. So we need to manually invoke our version of it.
                return exchangepull(orig, self, *args, **kwargs)
            else:
                return orig(self, *args, **kwargs)
        wrapfunction(exchange, 'pull', pull_shallow)

        # Wrap the stream logic to add requirements and to pass include/exclude
        # patterns around.
        def setup_streamout(repo, remote):
            # Replace remote.stream_out with a version that sends file
            # patterns.
            def stream_out_shallow(orig):
                if shallowrepo.requirement in remote._capabilities():
                    opts = {}
                    if repo.includepattern:
                        opts['includepattern'] = '\0'.join(repo.includepattern)
                    if repo.excludepattern:
                        opts['excludepattern'] = '\0'.join(repo.excludepattern)
                    return remote._callstream('stream_out_shallow', **opts)
                else:
                    return orig()
            wrapfunction(remote, 'stream_out', stream_out_shallow)
        if hasstreamclone:
            def stream_wrap(orig, op):
                setup_streamout(op.repo, op.remote)
                return orig(op)
            wrapfunction(streamclone, 'maybeperformlegacystreamclone',
                         stream_wrap)

            def canperformstreamclone(orig, *args, **kwargs):
                supported, requirements = orig(*args, **kwargs)
                if requirements is not None:
                    requirements.add(shallowrepo.requirement)
                return supported, requirements
            wrapfunction(streamclone, 'canperformstreamclone',
                         canperformstreamclone)
        else:
            def stream_in_shallow(orig, repo, remote, requirements):
                setup_streamout(repo, remote)
                requirements.add(shallowrepo.requirement)
                return orig(repo, remote, requirements)
            wrapfunction(localrepo.localrepository, 'stream_in',
                         stream_in_shallow)

    try:
        orig(ui, repo, *args, **opts)
    finally:
        if opts.get('shallow'):
            for r in repos:
                if util.safehasattr(r, 'fileservice'):
                    r.fileservice.close()

def debugdatashallow(orig, *args, **kwds):
    oldlen = remotefilelog.remotefilelog.__len__
    try:
        remotefilelog.remotefilelog.__len__ = lambda x: 1
        return orig(*args, **kwds)
    finally:
        remotefilelog.remotefilelog.__len__ = oldlen

def reposetup(ui, repo):
    if not isinstance(repo, localrepo.localrepository):
        return

    # put here intentionally bc doesnt work in uisetup
    ui.setconfig('hooks', 'update.prefetch', wcpprefetch)
    ui.setconfig('hooks', 'commit.prefetch', wcpprefetch)

    isserverenabled = ui.configbool('remotefilelog', 'server')
    isshallowclient = shallowrepo.requirement in repo.requirements

    if isserverenabled and isshallowclient:
        raise RuntimeError("Cannot be both a server and shallow client.")

    if isshallowclient:
        setupclient(ui, repo)

    if isserverenabled:
        remotefilelogserver.setupserver(ui, repo)

def setupclient(ui, repo):
    if not isinstance(repo, localrepo.localrepository):
        return

    # Even clients get the server setup since they need to have the
    # wireprotocol endpoints registered.
    remotefilelogserver.onetimesetup(ui)
    onetimeclientsetup(ui)

    shallowrepo.wraprepo(repo)
    repo.store = shallowstore.wrapstore(repo.store)

clientonetime = False
def onetimeclientsetup(ui):
    global clientonetime
    if clientonetime:
        return
    clientonetime = True

    # some users in core still call changegroup.cg1packer directly
    changegroup.cg1packer = shallowbundle.shallowcg1packer

    packermap = None
    if util.safehasattr(changegroup, 'packermap'):
        packermap = changegroup.packermap
    elif util.safehasattr(changegroup, '_packermap'):
        packermap = changegroup._packermap

    if packermap:
        # Mercurial >= 3.3
        packermap01 = packermap['01']
        packermap02 = packermap['02']
        packermap['01'] = (shallowbundle.shallowcg1packer,
                                       packermap01[1])
        packermap['02'] = (shallowbundle.shallowcg2packer,
                                       packermap02[1])
    if util.safehasattr(changegroup, '_addchangegroupfiles'):
        fn = '_addchangegroupfiles' # hg >= 3.6
    else:
        fn = 'addchangegroupfiles' # hg <= 3.5
    wrapfunction(changegroup, fn, shallowbundle.addchangegroupfiles)
    wrapfunction(changegroup, 'getchangegroup', shallowbundle.getchangegroup)

    def storewrapper(orig, requirements, path, vfstype):
        s = orig(requirements, path, vfstype)
        if shallowrepo.requirement in requirements:
            s = shallowstore.wrapstore(s)

        return s
    wrapfunction(store, 'store', storewrapper)

    extensions.wrapfunction(exchange, 'pull', exchangepull)

    # prefetch files before update
    def applyupdates(orig, repo, actions, wctx, mctx, overwrite, labels=None):
        if shallowrepo.requirement in repo.requirements:
            manifest = mctx.manifest()
            files = []
            for f, args, msg in actions['g']:
                files.append((f, hex(manifest[f])))
            # batch fetch the needed files from the server
            repo.fileservice.prefetch(files)
        return orig(repo, actions, wctx, mctx, overwrite, labels=labels)
    wrapfunction(merge, 'applyupdates', applyupdates)

    # Prefetch merge checkunknownfiles
    def checkunknownfiles(orig, repo, wctx, mctx, force, actions,
                          *args, **kwargs):
        if shallowrepo.requirement in repo.requirements:
            files = []
            sparsematch = repo.maybesparsematch(mctx.rev())
            for f, (m, actionargs, msg) in actions.iteritems():
                if sparsematch and not sparsematch(f):
                    continue
                if m in ('c', 'dc', 'cm'):
                    files.append((f, hex(mctx.filenode(f))))
                elif m == 'dg':
                    f2 = actionargs[0]
                    files.append((f2, hex(mctx.filenode(f2))))
            # batch fetch the needed files from the server
            repo.fileservice.prefetch(files)
        return orig(repo, wctx, mctx, force, actions, *args, **kwargs)
    wrapfunction(merge, '_checkunknownfiles', checkunknownfiles)

    # Prefetch files before status attempts to look at their size and contents
    def checklookup(orig, self, files):
        repo = self._repo
        if shallowrepo.requirement in repo.requirements:
            prefetchfiles = []
            for parent in self._parents:
                for f in files:
                    if f in parent:
                        prefetchfiles.append((f, hex(parent.filenode(f))))
            # batch fetch the needed files from the server
            repo.fileservice.prefetch(prefetchfiles)
        return orig(self, files)
    wrapfunction(context.workingctx, '_checklookup', checklookup)

    # Prefetch the logic that compares added and removed files for renames
    def findrenames(orig, repo, matcher, added, removed, *args, **kwargs):
        if shallowrepo.requirement in repo.requirements:
            files = []
            parentctx = repo['.']
            for f in removed:
                files.append((f, hex(parentctx.filenode(f))))
            # batch fetch the needed files from the server
            repo.fileservice.prefetch(files)
        return orig(repo, matcher, added, removed, *args, **kwargs)
    wrapfunction(scmutil, '_findrenames', findrenames)

    # prefetch files before mergecopies check
    def computenonoverlap(orig, repo, c1, c2, *args, **kwargs):
        u1, u2 = orig(repo, c1, c2, *args, **kwargs)
        if shallowrepo.requirement in repo.requirements:
            m1 = c1.manifest()
            m2 = c2.manifest()
            files = []

            sparsematch1 = repo.maybesparsematch(c1.rev())
            if sparsematch1:
                sparseu1 = []
                for f in u1:
                    if sparsematch1(f):
                        files.append((f, hex(m1[f])))
                        sparseu1.append(f)
                u1 = sparseu1

            sparsematch2 = repo.maybesparsematch(c2.rev())
            if sparsematch2:
                sparseu2 = []
                for f in u2:
                    if sparsematch2(f):
                        files.append((f, hex(m2[f])))
                        sparseu2.append(f)
                u2 = sparseu2

            # batch fetch the needed files from the server
            repo.fileservice.prefetch(files)
        return u1, u2
    wrapfunction(copies, '_computenonoverlap', computenonoverlap)

    # prefetch files before pathcopies check
    def computeforwardmissing(orig, a, b, match=None):
        missing = list(orig(a, b, match=match))
        repo = a._repo
        if shallowrepo.requirement in repo.requirements:
            mb = b.manifest()

            files = []
            sparsematch = repo.maybesparsematch(b.rev())
            if sparsematch:
                sparsemissing = []
                for f in missing:
                    if sparsematch(f):
                        files.append((f, hex(mb[f])))
                        sparsemissing.append(f)
                missing = sparsemissing

            # batch fetch the needed files from the server
            repo.fileservice.prefetch(files)
        return missing
    wrapfunction(copies, '_computeforwardmissing', computeforwardmissing)

    # close cache miss server connection after the command has finished
    def runcommand(orig, lui, repo, *args, **kwargs):
        try:
            return orig(lui, repo, *args, **kwargs)
        finally:
            # repo can be None when running in chg:
            # - at startup, reposetup was called because serve is not norepo
            # - a norepo command like "help" is called
            if repo and shallowrepo.requirement in repo.requirements:
                repo.fileservice.close()
    wrapfunction(dispatch, 'runcommand', runcommand)

    # disappointing hacks below
    templatekw.getrenamedfn = getrenamedfn
    wrapfunction(revset, 'filelog', filelogrevset)
    revset.symbols['filelog'] = revset.filelog
    wrapfunction(cmdutil, 'walkfilerevs', walkfilerevs)

    # prevent strip from stripping remotefilelogs
    def _collectbrokencsets(orig, repo, files, striprev):
        if shallowrepo.requirement in repo.requirements:
            files = list([f for f in files if not repo.shallowmatch(f)])
        return orig(repo, files, striprev)
    wrapfunction(repair, '_collectbrokencsets', _collectbrokencsets)

    # Don't commit filelogs until we know the commit hash, since the hash
    # is present in the filelog blob.
    # This violates Mercurial's filelog->manifest->changelog write order,
    # but is generally fine for client repos.
    pendingfilecommits = []
    def addrawrevision(orig, self, rawtext, transaction, link, p1, p2, node,
                       flags, cachedelta=None, _metatuple=None):
        if isinstance(link, int):
            pendingfilecommits.append(
                (self, rawtext, transaction, link, p1, p2, node, flags,
                 cachedelta, _metatuple))
            return node
        else:
            return orig(self, rawtext, transaction, link, p1, p2, node, flags,
                        cachedelta, _metatuple=_metatuple)
    wrapfunction(remotefilelog.remotefilelog, 'addrawrevision', addrawrevision)

    def changelogadd(orig, self, *args):
        oldlen = len(self)
        node = orig(self, *args)
        newlen = len(self)
        if oldlen != newlen:
            for oldargs in pendingfilecommits:
                log, rt, tr, link, p1, p2, n, fl, c, m = oldargs
                linknode = self.node(link)
                if linknode == node:
                    log.addrawrevision(rt, tr, linknode, p1, p2, n, fl, c, m)
                else:
                    raise error.ProgrammingError(
                        'pending multiple integer revisions are not supported')
        else:
            # "link" is actually wrong here (it is set to len(changelog))
            # if changelog remains unchanged, skip writing file revisions
            # but still do a sanity check about pending multiple revisions
            if len(set(x[3] for x in pendingfilecommits)) > 1:
                raise error.ProgrammingError(
                    'pending multiple integer revisions are not supported')
        del pendingfilecommits[:]
        return node
    wrapfunction(changelog.changelog, 'add', changelogadd)

    # changectx wrappers
    def filectx(orig, self, path, fileid=None, filelog=None):
        if fileid is None:
            fileid = self.filenode(path)
        if (shallowrepo.requirement in self._repo.requirements and
            self._repo.shallowmatch(path)):
            return remotefilectx.remotefilectx(self._repo, path,
                fileid=fileid, changectx=self, filelog=filelog)
        return orig(self, path, fileid=fileid, filelog=filelog)
    wrapfunction(context.changectx, 'filectx', filectx)

    def workingfilectx(orig, self, path, filelog=None):
        if (shallowrepo.requirement in self._repo.requirements and
            self._repo.shallowmatch(path)):
            return remotefilectx.remoteworkingfilectx(self._repo,
                path, workingctx=self, filelog=filelog)
        return orig(self, path, filelog=filelog)
    wrapfunction(context.workingctx, 'filectx', workingfilectx)

    # prefetch required revisions before a diff
    def trydiff(orig, repo, revs, ctx1, ctx2, modified, added, removed,
                copy, getfilectx, *args, **kwargs):
        if shallowrepo.requirement in repo.requirements:
            prefetch = []
            mf1 = ctx1.manifest()
            for fname in modified + added + removed:
                if fname in mf1:
                    fnode = getfilectx(fname, ctx1).filenode()
                    # fnode can be None if it's a edited working ctx file
                    if fnode:
                        prefetch.append((fname, hex(fnode)))
                if fname not in removed:
                    fnode = getfilectx(fname, ctx2).filenode()
                    if fnode:
                        prefetch.append((fname, hex(fnode)))

            repo.fileservice.prefetch(prefetch)

        return orig(repo, revs, ctx1, ctx2, modified, added, removed,
            copy, getfilectx, *args, **kwargs)
    wrapfunction(patch, 'trydiff', trydiff)

    # Prevent verify from processing files
    # a stub for mercurial.hg.verify()
    def _verify(orig, repo):
        lock = repo.lock()
        try:
            return shallowverifier.shallowverifier(repo).verify()
        finally:
            lock.release()

    wrapfunction(hg, 'verify', _verify)

    if util.safehasattr(cmdutil, '_revertprefetch'):
        wrapfunction(cmdutil, '_revertprefetch', _revertprefetch)
    else:
        wrapfunction(cmdutil, 'revert', revert)

def getrenamedfn(repo, endrev=None):
    rcache = {}

    def getrenamed(fn, rev):
        '''looks up all renames for a file (up to endrev) the first
        time the file is given. It indexes on the changerev and only
        parses the manifest if linkrev != changerev.
        Returns rename info for fn at changerev rev.'''
        if rev in rcache.setdefault(fn, {}):
            return rcache[fn][rev]

        try:
            fctx = repo[rev].filectx(fn)
            for ancestor in fctx.ancestors():
                if ancestor.path() == fn:
                    renamed = ancestor.renamed()
                    rcache[fn][ancestor.rev()] = renamed

            return fctx.renamed()
        except error.LookupError:
            return None

    return getrenamed

def walkfilerevs(orig, repo, match, follow, revs, fncache):
    if not shallowrepo.requirement in repo.requirements:
        return orig(repo, match, follow, revs, fncache)

    # remotefilelog's can't be walked in rev order, so throw.
    # The caller will see the exception and walk the commit tree instead.
    if not follow:
        raise cmdutil.FileWalkError("Cannot walk via filelog")

    wanted = set()
    minrev, maxrev = min(revs), max(revs)

    pctx = repo['.']
    for filename in match.files():
        if filename not in pctx:
            raise error.Abort(_('cannot follow file not in parent '
                               'revision: "%s"') % filename)
        fctx = pctx[filename]

        linkrev = fctx.linkrev()
        if linkrev >= minrev and linkrev <= maxrev:
            fncache.setdefault(linkrev, []).append(filename)
            wanted.add(linkrev)

        for ancestor in fctx.ancestors():
            linkrev = ancestor.linkrev()
            if linkrev >= minrev and linkrev <= maxrev:
                fncache.setdefault(linkrev, []).append(ancestor.path())
                wanted.add(linkrev)

    return wanted

def filelogrevset(orig, repo, subset, x):
    """``filelog(pattern)``
    Changesets connected to the specified filelog.

    For performance reasons, ``filelog()`` does not show every changeset
    that affects the requested file(s). See :hg:`help log` for details. For
    a slower, more accurate result, use ``file()``.
    """

    if not shallowrepo.requirement in repo.requirements:
        return orig(repo, subset, x)

    # i18n: "filelog" is a keyword
    pat = revset.getstring(x, _("filelog requires a pattern"))
    m = match.match(repo.root, repo.getcwd(), [pat], default='relpath',
                       ctx=repo[None])
    s = set()

    if not match.patkind(pat):
        # slow
        for r in subset:
            ctx = repo[r]
            cfiles = ctx.files()
            for f in m.files():
                if f in cfiles:
                    s.add(ctx.rev())
                    break
    else:
        # partial
        files = (f for f in repo[None] if m(f))
        for f in files:
            fctx = repo[None].filectx(f)
            s.add(fctx.linkrev())
            for actx in fctx.ancestors():
                s.add(actx.linkrev())

    return smartset.baseset([r for r in subset if r in s])

@command('gc', [], _('hg gc [REPO...]'), norepo=True)
def gc(ui, *args, **opts):
    '''garbage collect the client and server filelog caches
    '''
    cachepaths = set()

    # get the system client cache
    systemcache = shallowutil.getcachepath(ui, allowempty=True)
    if systemcache:
        cachepaths.add(systemcache)

    # get repo client and server cache
    repopaths = []
    pwd = ui.environ.get('PWD')
    if pwd:
        repopaths.append(pwd)

    repopaths.extend(args)
    repos = []
    for repopath in repopaths:
        try:
            repo = hg.peer(ui, {}, repopath)
            repos.append(repo)

            repocache = shallowutil.getcachepath(repo.ui, allowempty=True)
            if repocache:
                cachepaths.add(repocache)
        except error.RepoError:
            pass

    # gc client cache
    for cachepath in cachepaths:
        gcclient(ui, cachepath)

    # gc server cache
    for repo in repos:
        remotefilelogserver.gcserver(ui, repo._repo)

def gcclient(ui, cachepath):
    # get list of repos that use this cache
    repospath = os.path.join(cachepath, 'repos')
    if not os.path.exists(repospath):
        ui.warn(_("no known cache at %s\n") % cachepath)
        return

    reposfile = open(repospath, 'r')
    repos = set([r[:-1] for r in reposfile.readlines()])
    reposfile.close()

    # build list of useful files
    validrepos = []
    keepkeys = set()

    _analyzing = _("analyzing repositories")

    sharedcache = None
    filesrepacked = False

    count = 0
    for path in repos:
        ui.progress(_analyzing, count, unit="repos", total=len(repos))
        count += 1
        try:
            path = ui.expandpath(os.path.normpath(path))
        except TypeError as e:
            ui.warn(_("warning: malformed path: %r:%s\n") % (path, e))
            traceback.print_exc()
            continue
        try:
            peer = hg.peer(ui, {}, path)
            repo = peer._repo
        except error.RepoError:
            continue

        validrepos.append(path)

        # Protect against any repo or config changes that have happened since
        # this repo was added to the repos file. We'd rather this loop succeed
        # and too much be deleted, than the loop fail and nothing gets deleted.
        if shallowrepo.requirement not in repo.requirements:
            continue

        if not util.safehasattr(repo, 'name'):
            ui.warn(_("repo %s is a misconfigured remotefilelog repo\n") % path)
            continue

        # If garbage collection on repack and repack on hg gc are enabled
        # then loose files are repacked and garbage collected.
        # Otherwise regular garbage collection is performed.
        repackonhggc = repo.ui.configbool('remotefilelog', 'repackonhggc')
        gcrepack = repo.ui.configbool('remotefilelog', 'gcrepack')
        if repackonhggc and gcrepack:
            try:
                repackmod.incrementalrepack(repo)
                filesrepacked = True
                continue
            except IOError:
                # If repack cannot be performed due to not enough disk space
                # continue doing garbage collection of loose files w/o repack
                pass

        reponame = repo.name
        if not sharedcache:
            sharedcache = repo.sharedstore

        # Compute a keepset which is not garbage collected
        # We want to keep:
        # 1. Working copy parent
        # 2. Draft commits
        # 3. All parents of draft commits
        # 4. Recent heads in the repo
        # 5. Pullprefetch and bgprefetchrevs revsets if specified
        revs = ['.', 'draft()', 'parents(draft())', '(heads(all()) & date(-7))']

        prefetchrevs = repo.ui.config('remotefilelog',
                                      'pullprefetch', None)
        if prefetchrevs:
            revs.append('(%s)' % prefetchrevs)
        prefetchrevs = repo.ui.config('remotefilelog',
                                      'bgprefetchrevs', None)
        if prefetchrevs:
            revs.append('(%s)' % prefetchrevs)

        keep = scmutil.revrange(repo, ['+'.join(revs)])
        # TODO: Compute keepkeys more efficiently
        for r in keep:
            m = repo[r].manifest()
            for filename, filenode in m.iteritems():
                key = fileserverclient.getcachekey(reponame, filename,
                    hex(filenode))
                keepkeys.add(key)

    ui.progress(_analyzing, None)

    # write list of valid repos back
    oldumask = os.umask(0o002)
    try:
        reposfile = open(repospath, 'w')
        reposfile.writelines([("%s\n" % r) for r in validrepos])
        reposfile.close()
    finally:
        os.umask(oldumask)

    # prune cache
    if sharedcache is not None:
        sharedcache.gc(keepkeys)
    elif not filesrepacked:
        ui.warn(_("warning: no valid repos in repofile\n"))


def log(orig, ui, repo, *pats, **opts):
    if shallowrepo.requirement not in repo.requirements:
        return orig(ui, repo, *pats, **opts)

    follow = opts.get('follow')
    revs = opts.get('rev')
    if pats:
        # Force slowpath for non-follow patterns and follows that start from
        # non-working-copy-parent revs.
        if not follow or revs:
            # This forces the slowpath
            opts['removed'] = True

        # If this is a non-follow log without any revs specified, recommend that
        # the user add -f to speed it up.
        if not follow and not revs:
            match, pats = scmutil.matchandpats(repo['.'], pats, opts)
            isfile = not match.anypats()
            if isfile:
                for file in match.files():
                    if not os.path.isfile(repo.wjoin(file)):
                        isfile = False
                        break

            if isfile:
                ui.warn(_("warning: file log can be slow on large repos - " +
                          "use -f to speed it up\n"))

    return orig(ui, repo, *pats, **opts)

def wcpprefetch(ui, repo, **kwargs):
    """Prefetches in background revisions specified by bgprefetchrevs revset.
    Does background repack if backgroundrepack flag is set in config.
    """
    shallow = shallowrepo.requirement in repo.requirements
    bgprefetchrevs = ui.config('remotefilelog', 'bgprefetchrevs', None)

    if shallow and bgprefetchrevs:
        bgrepack = repo.ui.configbool('remotefilelog',
                                      'backgroundrepack', False)
        def anon():
            if util.safehasattr(repo, 'ranprefetch') and repo.ranprefetch:
                return
            repo.ranprefetch = True
            repo.backgroundprefetch(bgprefetchrevs, repack=bgrepack)

        repo._afterlock(anon)

def pull(orig, ui, repo, *pats, **opts):
    result = orig(ui, repo, *pats, **opts)

    if shallowrepo.requirement in repo.requirements:
        # prefetch if it's configured
        prefetchrevset = ui.config('remotefilelog', 'pullprefetch', None)
        bgrepack = repo.ui.configbool('remotefilelog',
                                      'backgroundrepack', False)
        bgprefetch = repo.ui.configbool('remotefilelog',
                                        'backgroundprefetch', False)

        if prefetchrevset:
            ui.status(_("prefetching file contents\n"))
            revs = scmutil.revrange(repo, [prefetchrevset])
            base = repo['.'].rev()
            if bgprefetch:
                repo.backgroundprefetch(prefetchrevset, repack=bgrepack)
            else:
                repo.prefetch(revs, base=base)
                if bgrepack:
                    repackmod.backgroundrepack(repo, incremental=True)
        elif bgrepack:
            repackmod.backgroundrepack(repo, incremental=True)

    return result

def exchangepull(orig, repo, remote, *args, **kwargs):
    # Hook into the callstream/getbundle to insert bundle capabilities
    # during a pull.
    def localgetbundle(orig, source, heads=None, common=None, bundlecaps=None,
                       **kwargs):
        if not bundlecaps:
            bundlecaps = set()
        bundlecaps.add('remotefilelog')
        return orig(source, heads=heads, common=common, bundlecaps=bundlecaps,
                    **kwargs)

    if util.safehasattr(remote, '_callstream'):
        remote._localrepo = repo
    elif util.safehasattr(remote, 'getbundle'):
        wrapfunction(remote, 'getbundle', localgetbundle)

    return orig(repo, remote, *args, **kwargs)

def revert(orig, ui, repo, ctx, parents, *pats, **opts):
    # prefetch prior to reverting
    # used for old mercurial version
    if shallowrepo.requirement in repo.requirements:
        files = []
        m = scmutil.match(ctx, pats, opts)
        mf = ctx.manifest()
        m.bad = lambda x, y: False
        for path in ctx.walk(m):
            files.append((path, hex(mf[path])))
        repo.fileservice.prefetch(files)

    return orig(ui, repo, ctx, parents, *pats, **opts)

def _revertprefetch(orig, repo, ctx, *files):
    # prefetch data that needs to be reverted
    # used for new mercurial version
    if shallowrepo.requirement in repo.requirements:
        allfiles = []
        mf = ctx.manifest()
        sparsematch = repo.maybesparsematch(ctx.rev())
        for f in files:
            for path in f:
                if (not sparsematch or sparsematch(path)) and path in mf:
                    allfiles.append((path, hex(mf[path])))
        repo.fileservice.prefetch(allfiles)
    return orig(repo, ctx, *files)

@command('debugremotefilelog', [
    ('d', 'decompress', None, _('decompress the filelog first')),
    ], _('hg debugremotefilelog <path>'), norepo=True)
def debugremotefilelog(ui, path, **opts):
    return debugcommands.debugremotefilelog(ui, path, **opts)

@command('verifyremotefilelog', [
    ('d', 'decompress', None, _('decompress the filelogs first')),
    ], _('hg verifyremotefilelogs <directory>'), norepo=True)
def verifyremotefilelog(ui, path, **opts):
    return debugcommands.verifyremotefilelog(ui, path, **opts)

@command('debugdatapack', [
    ('', 'long', None, _('print the long hashes')),
    ('', 'node', '', _('dump the contents of node'), 'NODE'),
    ], _('hg debugdatapack <path>'), norepo=True)
def debugdatapack(ui, path, **opts):
    return debugcommands.debugdatapack(ui, path, **opts)

@command('debughistorypack', [
    ], _('hg debughistorypack <path>'), norepo=True)
def debughistorypack(ui, path, **opts):
    return debugcommands.debughistorypack(ui, path)

@command('debugwaitonrepack', [
    ], _('hg debugwaitonrepack'))
def debugwaitonrepack(ui, repo, **opts):
    return debugcommands.debugwaitonrepack(repo)

@command('debugwaitonprefetch', [
    ], _('hg debugwaitonprefetch'))
def debugwaitonprefetch(ui, repo, **opts):
    return debugcommands.debugwaitonprefetch(repo)

@command('prefetch', [
    ('r', 'rev', [], _('prefetch the specified revisions'), _('REV')),
    ('', 'repack', False, _('run repack after prefetch')),
    ] + commands.walkopts, _('hg prefetch [OPTIONS] [FILE...]'))
def prefetch(ui, repo, *pats, **opts):
    """prefetch file revisions from the server

    Prefetchs file revisions for the specified revs and stores them in the
    local remotefilelog cache.  If no rev is specified, the default rev is
    used which is the union of dot, draft, pullprefetch and bgprefetchrev.
    File names or patterns can be used to limit which files are downloaded.

    Return 0 on success.
    """
    if not shallowrepo.requirement in repo.requirements:
        raise error.Abort(_("repo is not shallow"))

    if not opts.get('rev'):
        revset = ['.', 'draft()']

        prefetchrevset = ui.config('remotefilelog', 'pullprefetch', None)
        if prefetchrevset:
            revset.append('(%s)' % prefetchrevset)
        bgprefetchrevs = ui.config('remotefilelog', 'bgprefetchrevs', None)
        if bgprefetchrevs:
            revset.append('(%s)' % bgprefetchrevs)

        opts['rev'] = ['+'.join(revset)]

    revs = scmutil.revrange(repo, opts.get('rev'))

    repo.prefetch(revs, repack=opts.get('repack'), pats=pats, opts=opts)

@command('repack', [
     ('', 'background', None, _('run in a background process'), None),
     ('', 'incremental', None, _('do an incremental repack'), None),
    ], _('hg repack [OPTIONS]'))
def repack(ui, repo, *pats, **opts):
    if opts.get('background'):
        repackmod.backgroundrepack(repo, incremental=opts.get('incremental'))
        return

    if opts.get('incremental'):
        repackmod.incrementalrepack(repo)
    else:
        repackmod.fullrepack(repo)
