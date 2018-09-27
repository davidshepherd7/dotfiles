# pushrebase.py - server-side rebasing of pushed changesets
#
# Copyright 2014 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.
"""rebases commits during push

The pushrebase extension allows the server to rebase incoming commits as part of
the push process. This helps solve the problem of push contention where many
clients try to push at once and all but one fail. Instead of failing, the
pushrebase extension will rebase the incoming commit onto the target bookmark
(i.e. @ or master) as long as the commit doesn't touch any files that have been
modified in the target bookmark. Put another way, pushrebase will not perform
any file content merges. It only performs the rebase when there is no chance of
a file merge.

Configs:

    ``pushrebase.forcetreereceive`` forces pushrebase to read incoming
    treemanifests instead of incoming flat manifests. This is useful for the
    transition to treemanifest.

"""
from __future__ import absolute_import

import errno, os, tempfile, mmap, time

from mercurial import (
    bundle2,
    changegroup,
    commands,
    context,
    discovery,
    encoding,
    error,
    exchange,
    extensions,
    hg,
    manifest,
    obsolete,
    phases as phasesmod,
    pushkey,
    registrar,
    revsetlang,
    scmutil,
    util,
)
from mercurial.extensions import wrapcommand, wrapfunction, unwrapfunction
from mercurial.node import nullid, hex, bin
from mercurial.i18n import _

from remotefilelog import (
    contentstore,
    datapack,
    historypack,
    metadatastore,
    wirepack,
)

testedwith = 'ships-with-fb-hgext'

cmdtable = {}
command = registrar.command(cmdtable)

configtable = {}
configitem = registrar.configitem(configtable)

configitem('pushrebase', 'blocknonpushrebase', default=False)

rebaseparttype = 'b2x:rebase'
rebasepackparttype = 'b2x:rebasepackpart'
commonheadsparttype = 'b2x:commonheads'

treepackrecords = 'tempmanifestspackdir'

experimental = 'experimental'
configonto = 'server-rebase-onto'
pushrebasemarker = '__pushrebase_processed__'
donotrebasemarker = '__pushrebase_donotrebase__'

def uisetup(ui):
    # remotenames circumvents the default push implementation entirely, so make
    # sure we load after it so that we wrap it.
    order = extensions._order
    order.remove('pushrebase')
    order.append('pushrebase')
    extensions._order = order

def extsetup(ui):
    entry = wrapcommand(commands.table, 'push', _push)
    try:
        # Don't add the 'to' arg if it already exists
        extensions.find('remotenames')
    except KeyError:
        entry[1].append(('', 'to', '', _('server revision to rebase onto')))

    partorder = exchange.b2partsgenorder

    # rebase part must go before the changeset part, so we can mark the
    # changeset part as done first.
    partorder.insert(partorder.index('changeset'),
                     partorder.pop(partorder.index(rebaseparttype)))

    # rebase pack part must go before rebase part so it can write to the pack to
    # disk for reading.
    partorder.insert(partorder.index(rebaseparttype),
                     partorder.pop(partorder.index(rebasepackparttype)))

    partorder.insert(0, partorder.pop(partorder.index(commonheadsparttype)))

    if 'check-bookmarks' in partorder:
        # check-bookmarks is intended for non-pushrebase scenarios when
        # we can't push to a bookmark if it's changed in the meantime
        partorder.pop(partorder.index('check-bookmarks'))

    wrapfunction(discovery, 'checkheads', _checkheads)
    # we want to disable the heads check because in pushrebase repos, we
    # expect the heads to change during the push and we should not abort.

    # The check heads functions are used to verify that the heads haven't
    # changed since the client did the initial discovery. Pushrebase is meant
    # to allow concurrent pushes, so the heads may have very well changed.
    # So let's not do this check.
    wrapfunction(exchange, 'check_heads', _exchangecheckheads)
    wrapfunction(exchange, '_pushb2ctxcheckheads', _skipcheckheads)

    origpushkeyhandler = bundle2.parthandlermapping['pushkey']
    newpushkeyhandler = lambda *args, **kwargs: \
        bundle2pushkey(origpushkeyhandler, *args, **kwargs)
    newpushkeyhandler.params = origpushkeyhandler.params
    bundle2.parthandlermapping['pushkey'] = newpushkeyhandler
    bundle2.parthandlermapping['b2x:pushkey'] = newpushkeyhandler

    origphaseheadshandler = bundle2.parthandlermapping['phase-heads']
    newphaseheadshandler = lambda *args, **kwargs: \
        bundle2phaseheads(origphaseheadshandler, *args, **kwargs)
    newphaseheadshandler.params = origphaseheadshandler.params
    bundle2.parthandlermapping['phase-heads'] = newphaseheadshandler

    wrapfunction(exchange, 'unbundle', unbundle)

    wrapfunction(hg, '_peerorrepo', _peerorrepo)

def reposetup(ui, repo):
    if repo.ui.configbool('pushrebase', 'blocknonpushrebase'):
        repo.ui.setconfig('hooks', 'prechangegroup.blocknonpushrebase',
                          blocknonpushrebase)

    # https://www.mercurial-scm.org/repo/hg/rev/a1e70c1dbec0
    # and related commits added a new way to pushing bookmarks
    # Since pushrebase for now uses pushkey, we want to set this config
    # (T24314128 tracks this)
    legexc = repo.ui.configlist('devel', 'legacy.exchange', [])
    if 'bookmarks' not in legexc:
        legexc.append('bookmarks')
    repo.ui.setconfig('devel', 'legacy.exchange', legexc, 'pushrebase')

def blocknonpushrebase(ui, repo, **kwargs):
    if not repo.ui.configbool('pushrebase', pushrebasemarker):
        raise error.Abort(_("this repository requires that you enable the "
                            "pushrebase extension and push using "
                            "'hg push --to'"))

def _peerorrepo(orig, ui, path, create=False, **kwargs):
    # Force hooks to use a bundle repo
    bundlepath = encoding.environ.get("HG_HOOK_BUNDLEPATH")
    if bundlepath:
        packpaths = encoding.environ.get("HG_HOOK_PACKPATHS")
        if packpaths:
            ui.setconfig("treemanifest", "treeonly", True)

        repo = orig(ui, bundlepath, create=create, **kwargs)

        # Add hook pack paths to the store
        if packpaths:
            bundledatastores = []
            bundlehiststores = []
            paths = packpaths.split(':')
            for path in paths:
                datastore, histstore = _createpackstore(repo.ui, path)
                bundledatastores.append(datastore)
                bundlehiststores.append(histstore)

            # Point the bundle repo at the temp stores
            repo.manifestlog.datastore = contentstore.unioncontentstore(
                repo.manifestlog.datastore,
                *bundledatastores)
            repo.manifestlog.historystore = metadatastore.unionmetadatastore(
                repo.manifestlog.historystore,
                *bundlehiststores)
        return repo

    return orig(ui, path, create, **kwargs)

def unbundle(orig, repo, cg, heads, source, url):
    # Preload the manifests that the client says we'll need. This happens
    # outside the lock, thus cutting down on our lock time and increasing commit
    # throughput.
    if util.safehasattr(cg, 'params'):
        preloadmfs = cg.params.get('preloadmanifests')
        if preloadmfs:
            for mfnode in preloadmfs.split(','):
                repo.manifestlog[bin(mfnode)].read()

    return orig(repo, cg, heads, source, url)

def validaterevset(repo, revset):
    "Abort if this is a rebasable revset, return None otherwise"
    if not repo.revs(revset):
        raise error.Abort(_('nothing to rebase'))

    revs = repo.revs('%r and public()', revset)
    if revs:
        nodes = []
        for count, rev in enumerate(revs):
            if count >= 3:
                nodes.append('...')
                break
            nodes.append(str(repo[rev]))
        revstring = ', '.join(nodes)
        raise error.Abort(_('cannot rebase public changesets: %s') % revstring)

    if repo.revs('%r and obsolete()', revset):
        raise error.Abort(_('cannot rebase obsolete changesets'))

    heads = repo.revs('heads(%r)', revset)
    if len(heads) > 1:
        raise error.Abort(_('cannot rebase divergent changesets'))

    repo.ui.note(_('validated revset for rebase\n'))

def getrebaseparts(repo, peer, outgoing, onto, newhead):
    parts = []
    if (util.safehasattr(repo.manifestlog, 'datastore') and
        repo.ui.configbool('treemanifest', 'sendtrees')):
        mfnodes = []
        for node in outgoing.missing:
            mfnodes.append(('', repo[node].manifestnode()))

        # Only add trees if we already have them
        if not repo.manifestlog.datastore.getmissing(mfnodes):
            parts.append(createtreepackpart(repo, outgoing))

    parts.append(createrebasepart(repo, peer, outgoing, onto, newhead))
    return parts

def createtreepackpart(repo, outgoing):
    treemod = extensions.find('treemanifest')
    return treemod.createtreepackpart(repo, outgoing, rebasepackparttype)

def createrebasepart(repo, peer, outgoing, onto, newhead):
    if not outgoing.missing:
        raise error.Abort(_('no changesets to rebase'))

    if rebaseparttype not in bundle2.bundle2caps(peer):
        raise error.Abort(_('no server support for %r') % rebaseparttype)

    validaterevset(repo, revsetlang.formatspec('%ln', outgoing.missing))

    cg = changegroup.makestream(repo, outgoing, '01', 'push')

    # Explicitly notify the server what obsmarker versions the client supports
    # so the client could receive marker from the server.
    #
    # The core mercurial logic will do the right thing (enable obsmarker
    # capabilities in the pushback bundle) if obsmarker exchange is enabled
    # client-side.
    #
    # But we want the marker without enabling marker exchange, and our server
    # could reply a marker without exchange or even obsstore enabled. So we
    # bypass the "standard" way of capabilities check by sending the supported
    # versions directly in our own part. Note: do not enable "exchange" because
    # it has an unwanted side effect: pushing markers from client to server.
    #
    # "createmarkers" is all we need to be able to write a new marker.
    if obsolete.isenabled(repo, obsolete.createmarkersopt):
        obsmarkerversions = '\0'.join(str(v) for v in obsolete.formats)
    else:
        obsmarkerversions = ''

    # .upper() marks this as a mandatory part: server will abort if there's no
    #  handler
    return bundle2.bundlepart(
        rebaseparttype.upper(),
        mandatoryparams={
            'onto': onto,
            'newhead': repr(newhead),
        }.items(),
        advisoryparams={
            # advisory: (old) server could ignore this without error
            'obsmarkerversions': obsmarkerversions,
        }.items(),
        data = cg)

def _checkheads(orig, pushop):
    repo = pushop.repo
    onto = repo.ui.config(experimental, configonto)
    if onto: # This is a rebasing push
        # If remotenames is enabled, we don't want to abort if the user uses
        # --to, even if the server doesn't support pushrebase.
        if checkremotenames():
            return

        # The rest of the checks are performed during bundle2 part processing;
        # we need to bypass the regular push checks because it will look like
        # we're pushing a new head, which isn't normally allowed
        if not repo.ui.configbool('experimental', 'bundle2-exp', False):
            raise error.Abort(_('bundle2 needs to be enabled on client'))
        if not pushop.remote.capable('bundle2-exp'):
            raise error.Abort(_('bundle2 needs to be enabled on server'))
        return
    else:
        return orig(pushop)

def _exchangecheckheads(orig, repo, *args, **kwargs):
    onto = repo.ui.config(experimental, configonto)
    if not onto:
        # Only do this work if it's not a rebasing push
        return orig(repo, *args, **kwargs)

def _skipcheckheads(orig, pushop, bundler):
    if not pushop.ui.config(experimental, configonto): # no check if we rebase
        return orig(pushop, bundler)

def _push(orig, ui, repo, *args, **opts):
    wnode = repo['.'].node()
    onto = opts.get('to')
    if not onto and not opts.get('rev') and not opts.get('dest'):
        try:
            # If it's a tracking bookmark, remotenames will push there,
            # so let's set that up as our --to.
            remotenames = extensions.find('remotenames')
            active = remotenames.bmactive(repo)
            tracking = remotenames._readtracking(repo)
            if active and active in tracking:
                track = tracking[active]
                path, book = remotenames.splitremotename(track)
                onto = book
        except KeyError:
            # No remotenames? No big deal.
            pass

    overrides = {(experimental, configonto): onto,
                 ('remotenames', 'allownonfastforward'): True}
    if onto:
        overrides[(experimental, 'bundle2.pushback')] = True
        wrapfunction(exchange, '_localphasemove', _phasemove)
        wrapfunction(obsolete.obsstore, 'mergemarkers', _mergemarkers)

    try:
        with ui.configoverride(overrides, 'pushrebase'):
            result = orig(ui, repo, *args, **opts)
    finally:
        if onto:
            unwrapfunction(exchange, '_localphasemove', _phasemove)
            unwrapfunction(obsolete.obsstore, 'mergemarkers', _mergemarkers)

    mapping = getattr(repo.obsstore, '_pushrebasereplaces', {})
    if mapping:
        with repo.wlock(), repo.lock(), repo.transaction('push') as tr:
            # move working copy parent
            if wnode in mapping:
                hg.update(repo, mapping[wnode])
            # move bookmarks
            bmarks = repo._bookmarks
            bmarkchanges = []
            for oldnode, newnode in mapping.items():
                bmarkchanges.extend((name, newnode)
                                    for name in repo.nodebookmarks(oldnode))
            if bmarkchanges:
                bmarks.applychanges(repo, tr, bmarkchanges)

    return result

def _mergemarkers(orig, self, transaction, data):
    """record new markers so we could know the correct nodes for _phasemove"""
    version, markers = obsolete._readmarkers(data)
    self._pushrebasereplaces = {}
    if version == obsolete._fm1version:
        # only support fm1 1:1 replacements for now, record prec -> sucs
        for prec, sucs, flags, meta, date, parents in markers:
            if len(sucs) == 1:
                self._pushrebasereplaces[prec] = sucs[0]
    return orig(self, transaction, data)

def _phasemove(orig, pushop, nodes, phase=phasesmod.public):
    """prevent original changesets from being marked public

    When marking changesets as public, we need to mark the replaced nodes
    returned from the server instead. This is done by looking at the new
    obsmarker we received during "_mergemarkers" and map old nodes to new ones.

    See exchange.push for the order of this and bundle2 pushback:

        _pushdiscovery(pushop)
        _pushbundle2(pushop)
            # bundle2 pushback is processed here, but the client receiving the
            # pushback cannot affect pushop.*heads (which affects phasemove),
            # because it only gets "repo", and creates a separate "op":
            bundle2.processbundle(pushop.repo, reply, trgetter)
        _pushchangeset(pushop)
        _pushsyncphase(pushop)
            _localphasemove(...) # this method always gets called
        _pushobsolete(pushop)
        _pushbookmark(pushop)

    The least hacky way to get things "right" seem to be:

        1. In core, allow bundle2 pushback handler to affect the original
           "pushop" somehow (so original pushop's (common|future)heads could be
           updated accordingly and phasemove logic is affected)
        2. In pushrebase extension, add a new bundle2 part handler to receive
           the new relationship, correct pushop.*headers, and write obsmarkers.
        3. Migrate the obsmarker part to the new bundle2 part added in step 2,
           i.e. the server won't send obsmarkers directly.

    For now, we don't have "1" so things are done in a bit hacky way.
    """
    # find replacements. note: _pushrebasereplaces could be empty if obsstore
    # is not enabled locally.
    mapping = getattr(pushop.repo.obsstore, '_pushrebasereplaces', {})
    nodes = [mapping.get(n, n) for n in nodes]
    if phase == phasesmod.public:
        # only allow new nodes to become public
        allowednodes = set(mapping.values())
        nodes = [n for n in nodes if n in allowednodes]
    orig(pushop, nodes, phase)

@exchange.b2partsgenerator(commonheadsparttype)
def commonheadspartgen(pushop, bundler):
    if rebaseparttype not in bundle2.bundle2caps(pushop.remote):
        # Server doesn't support pushrebase, so just fallback to normal push.
        return

    bundler.newpart(commonheadsparttype,
                    data=''.join(pushop.outgoing.commonheads))

@bundle2.parthandler(commonheadsparttype)
def commonheadshandler(op, inpart):
    nodeid = inpart.read(20)
    while len(nodeid) == 20:
        op.records.add(commonheadsparttype, nodeid)
        nodeid = inpart.read(20)
    assert not nodeid # data should split evenly into blocks of 20 bytes

def checkremotenames():
    try:
        extensions.find('remotenames')
        return True
    except KeyError:
        return False

@exchange.b2partsgenerator(rebasepackparttype)
def packpartgen(pushop, bundler):
    # We generate this part manually during pushrebase pushes, so this is a
    # no-op. But it's required because bundle2 expects there to be a generator
    # for every handler.
    pass

@exchange.b2partsgenerator(rebaseparttype)
def rebasepartgen(pushop, bundler):
    onto = pushop.ui.config(experimental, configonto)
    if 'changesets' in pushop.stepsdone or not onto:
        return

    if (rebaseparttype not in bundle2.bundle2caps(pushop.remote) and
        checkremotenames()):
        # Server doesn't support pushrebase, but --to is valid in remotenames as
        # well, so just let it through.
        return

    pushop.stepsdone.add('changesets')
    if not pushop.outgoing.missing:
        # It's important that this text match the text found in upstream
        # Mercurial, since some tools rely on this string to know if a push
        # succeeded despite not pushing commits.
        pushop.ui.status(_('no changes found\n'))
        pushop.cgresult = 0
        return

    # Force push means no rebasing, so let's just take the existing parent.
    if pushop.force:
        onto = donotrebasemarker

    rebaseparts = getrebaseparts(pushop.repo,
                                 pushop.remote,
                                 pushop.outgoing,
                                 onto,
                                 pushop.newbranch)

    for part in rebaseparts:
        bundler.addpart(part)

    # Tell the server which manifests to load before taking the lock.
    # This helps shorten the duration of the lock, which increases our potential
    # commit rate.
    missing = pushop.outgoing.missing
    roots = pushop.repo.set('parents(%ln) - %ln', missing, missing)
    preloadnodes = [hex(r.manifestnode()) for r in roots]
    bundler.addparam("preloadmanifests", ','.join(preloadnodes))

    def handlereply(op):
        # server either succeeds or aborts; no code to read
        pushop.cgresult = 1

    return handlereply

bundle2.capabilities[rebaseparttype] = ()

def _makebundlefile(part):
    """constructs a temporary bundle file

    part.data should be an uncompressed v1 changegroup"""

    fp = None
    fd, bundlefile = tempfile.mkstemp()
    try: # guards bundlefile
        try: # guards fp
            fp = os.fdopen(fd, 'wb')
            magic = 'HG10UN'
            fp.write(magic)
            data = part.read(mmap.PAGESIZE - len(magic))
            while data:
                fp.write(data)
                data = part.read(mmap.PAGESIZE)
        finally:
            fp.close()
    except Exception:
        try:
            os.unlink(bundlefile)
        except Exception:
            # we would rather see the original exception
            pass
        raise

    return bundlefile

def _getrenamesrcs(op, rev):
    '''get all rename sources in a revision'''
    srcs = set()
    revmf = _getmanifest(op, rev)
    for f in rev.files():
        if f in revmf:
            fctx = _getfilectx(rev, revmf, f)
            renamed = fctx.renamed()
            if renamed:
                srcs.add(renamed[0])
    return srcs

def _getrevs(op, bundle, onto, renamesrccache):
    'extracts and validates the revs to be imported'
    validaterevset(bundle, 'bundle()')
    revs = [bundle[r] for r in bundle.revs('sort(bundle())')]
    onto = bundle[onto.hex()]
    # Fast forward update, no rebase needed
    if list(bundle.set('bundle() & %d::', onto.rev())):
        return revs, onto

    if revs:
        # We want to rebase the highest bundle root that is an ancestor of
        # `onto`.
        oldonto = list(bundle.set('max(parents(bundle()) - bundle() & ::%d)',
                                  onto.rev()))
        if not oldonto:
            # If there's no shared history, only allow the rebase if the
            # incoming changes are completely distinct.
            sharedparents = list(bundle.set('parents(bundle()) - bundle()'))
            if not sharedparents:
                return revs, bundle[nullid]
            raise error.Abort(_('pushed changesets do not branch from an '
                                'ancestor of the desired destination %s')
                              % onto.hex())
        oldonto = oldonto[0]

        # Computes a list of all the incoming file changes
        bundlefiles = set()
        for bundlerev in revs:
            bundlefiles.update(bundlerev.files())

            # Also include sources of renames.
            bundlerevnode = bundlerev.node()
            if bundlerevnode in renamesrccache:
                bundlefiles.update(renamesrccache[bundlerevnode])
            else:
                bundlefiles.update(_getrenamesrcs(op, bundlerev))

        def findconflicts():
            # Returns all the files touched in the bundle that are also touched
            # between the old onto (ex: our old bookmark location) and the new
            # onto (ex: the server's actual bookmark location).
            filematcher = scmutil.matchfiles(bundle, bundlefiles)
            return onto.manifest().diff(oldonto.manifest(), filematcher).keys()

        def findconflictsfast():
            # Fast path for detecting conflicting files. Inspects the changelog
            # file list instead of loading manifests. This only works for
            # non-merge commits, since merge commit file lists do not include
            # all the files changed in the merged.
            ontofiles = set()
            for betweenctx in bundle.set('%d %% %d', onto.rev(), oldonto.rev()):
                ontofiles.update(betweenctx.files())

            return bundlefiles.intersection(ontofiles)

        if bundle.revs('(%d %% %d) - not merge()', onto.rev(), oldonto.rev()):
            # If anything between oldonto and newonto is a merge commit, use the
            # slower manifest diff path.
            conflicts = findconflicts()
        else:
            conflicts = findconflictsfast()

        if conflicts:
            msg = (_('conflicting changes in:\n%s\n') %
                    ''.join('    %s\n' % f for f in sorted(conflicts))).strip()
            hint = _('pull and rebase your changes locally, then try again')
            raise error.Abort(msg, hint=hint)

    return revs, oldonto

def _getmanifest(op, rev):
    repo = rev._repo
    if (not op.records[treepackrecords] and
        not repo.ui.configbool("pushrebase", "forcetreereceive")):
        m = rev.manifest()
    else:
        store = repo.manifestlog.datastore
        import cstore
        m = cstore.treemanifest(store, rev.manifestnode())
        if store.getmissing([('', rev.manifestnode())]):
            raise error.Abort(_('error: pushes must contain tree manifests '
                                'when the server has '
                                'pushrebase.forcetreereceive enabled'))
    return m

def _getfilectx(rev, mf, path):
    fileid = mf.get(path)
    return context.filectx(rev._repo, path, fileid=fileid,
                           changectx=rev)

def _graft(op, rev, mapping, lastdestnode):
    '''duplicate changeset "rev" with parents from "mapping"'''
    repo = op.repo
    oldp1 = rev.p1().node()
    oldp2 = rev.p2().node()
    newp1 = mapping.get(oldp1, oldp1)
    newp2 = mapping.get(oldp2, oldp2)

    m = _getmanifest(op, rev)

    def getfilectx(repo, memctx, path):
        if path in m:
            # We can't use the normal rev[path] accessor here since it will try
            # to go through the flat manifest, which may not exist.
            fctx = _getfilectx(rev, m, path)
            flags = m.flags(path)
            copied = fctx.renamed()
            if copied:
                copied = copied[0]
            return context.memfilectx(repo, memctx, fctx.path(), fctx.data(),
                              islink='l' in flags,
                              isexec='x' in flags,
                              copied=copied)
        else:
            return None


    # If the incoming commit has no parents, but requested a rebase,
    # allow it only for the first commit. The null/null commit will always
    # be the first commit since we only allow a nullid->nonnullid mapping if the
    # incoming commits are a completely distinct history (see `sharedparents` in
    # getrevs()), so there's no risk of commits with a single null parent
    # accidentally getting translated first.
    if oldp1 == nullid and oldp2 == nullid:
        if newp1 != nullid:
            newp2 = nullid
            del mapping[nullid]

    if oldp1 != nullid and oldp2 != nullid:
        # The way commits work is they copy p1, then apply the necessary changes
        # to get to the new state. In a pushrebase situation, we are applying
        # changes from the pre-rebase commit to a post-rebase commit, which
        # means we need to ensure that changes caused by the rebase are
        # preserved. In a merge commit, if p2 is the post-rebase commit that
        # contains all the files from the rebase destination, those changes will
        # be lost, since the newp1 doesn't have those changes, and
        # oldp1.diff(oldrev) doesn't have them either. The solution is to ensure
        # that the parent that contains all the original rebase destination
        # files is always p1. We do that by just swapping them here.
        if newp2 == lastdestnode:
            newtemp = newp1
            oldtemp = oldp1
            oldp1 = oldp2
            oldp2 = oldtemp
            newp1 = newp2
            newp2 = newtemp

        # If it's a merge commit, Mercurial's rev.files() only returns the files
        # that are different from both p1 and p2, so it would not capture all of
        # the incoming changes from p2 (for instance, new files in p2). The fix
        # is to manually diff the rev manifest and it's p1 to get the list of
        # files that have changed. We only need to diff against p1, and not p2,
        # because Mercurial constructs new commits by applying our specified
        # files on top of a copy of the p1 manifest, so we only need the diff
        # against p1.
        bundlerepo = rev._repo
        files = rev.manifest().diff(bundlerepo[oldp1].manifest()).keys()
    else:
        files = rev.files()


    date = rev.date()
    if repo.ui.configbool('pushrebase', 'rewritedates'):
        date = (time.time(), date[1])
    return context.memctx(repo,
                          [newp1, newp2],
                          rev.description(),
                          files,
                          getfilectx,
                          rev.user(),
                          date,
                          rev.extra(),
                         ).commit()

def _buildobsolete(replacements, oldrepo, newrepo, date):
    '''return obsmarkers, add them locally (server-side) if obsstore enabled'''
    markers = [(oldrepo[oldrev], (newrepo[newrev],),
                {'operation': 'push', 'user': newrepo[newrev].user()})
               for oldrev, newrev in replacements.items()
               if newrev != oldrev]
    if obsolete.isenabled(newrepo, obsolete.createmarkersopt):
        obsolete.createmarkers(newrepo, markers, date=date)
    return markers

def _addpushbackchangegroup(repo, reply, outgoing):
    '''adds changegroup part to reply containing revs from outgoing.missing'''
    cgversions = set(reply.capabilities.get('changegroup'))
    if not cgversions:
        cgversions.add('01')
    version = max(cgversions & set(changegroup.supportedoutgoingversions(repo)))

    cg = changegroup.makestream(repo,
                                outgoing,
                                version,
                                'rebase:reply')

    cgpart = reply.newpart('CHANGEGROUP', data=cg)
    if version != '01':
        cgpart.addparam('version', version)

def _addpushbackobsolete(repo, reply, markers, markerdate,
                         clientobsmarkerversions):
    '''adds obsmarkers to reply'''
    # experimental config: pushrebase.pushback.obsmarkers
    # if set to False, the server will not push back obsmarkers.
    if not repo.ui.configbool('pushrebase', 'pushback.obsmarkers', True):
        return

    # _buildobsolete has hard-coded obsolete._fm1version raw markers, so client
    # needs to support it, and the reply needs to have the correct capabilities
    if obsolete._fm1version not in clientobsmarkerversions:
        return
    reply.capabilities['obsmarkers'] = ['V1']

    flag = 0
    parents = None
    try:
        rawmarkers = [(pre.node(), tuple(s.node() for s in sucs), flag,
                       tuple(sorted(meta.items())), markerdate, parents)
                      for pre, sucs, meta in markers]
        bundle2.buildobsmarkerspart(reply, rawmarkers)
    except ValueError as exc:
        repo.ui.status(_("can't send obsolete markers: %s") % exc.message)

def _addpushbackparts(op, replacements, markers, markerdate,
                      clientobsmarkerversions):
    '''adds pushback to reply if supported by the client'''
    if (op.records[commonheadsparttype]
        and op.reply
        and 'pushback' in op.reply.capabilities):
        outgoing = discovery.outgoing(op.repo, op.records[commonheadsparttype],
                                      [new for old, new in replacements.items()
                                       if old != new])

        if outgoing.missing:
            plural = 's' if len(outgoing.missing) > 1 else ''
            op.repo.ui.warn(_("%s new changeset%s from the server will be "
                              "downloaded\n") % (len(outgoing.missing), plural))
            _addpushbackchangegroup(op.repo, op.reply, outgoing)
            _addpushbackobsolete(op.repo, op.reply, markers, markerdate,
                                 clientobsmarkerversions)

def resolveonto(repo, ontoarg):
    try:
        if ontoarg != donotrebasemarker:
            return scmutil.revsingle(repo, ontoarg)
    except error.RepoLookupError:
        # Probably a new bookmark. Leave onto as None to not do any rebasing
        pass
    # onto is None means don't do rebasing
    return None

@bundle2.parthandler(rebasepackparttype, ('version', 'cache', 'category'))
def packparthandler(op, part):
    repo = op.repo

    version = part.params.get('version')
    if version != '1':
        raise error.Abort(_("unknown rebasepack bundle2 part version: %s") %
                          version)

    temppackpath = tempfile.mkdtemp()
    op.records.add('tempdirs', temppackpath)
    wirepack.receivepack(repo.ui, part, temppackpath)
    op.records.add('temp%spackdir' % part.params.get('category', ''),
                   temppackpath)
    # TODO: clean up

def _createpackstore(ui, packpath):
    datastore = datapack.datapackstore(ui, packpath, usecdatapack=True)
    histstore = historypack.historypackstore(ui, packpath)
    return datastore, histstore

def _createbundlerepo(op, bundlepath):
    bundle = hg.repository(op.repo.ui, bundlepath)

    # Create stores for any received pack files
    bundledatastores = []
    bundlehiststores = []
    if op.records[treepackrecords]:
        for path in op.records[treepackrecords]:
            datastore, histstore = _createpackstore(op.repo.ui, path)
            bundledatastores.append(datastore)
            bundlehiststores.append(histstore)

        # Point the bundle repo at the temp stores
        bundle.manifestlog.datastore = contentstore.unioncontentstore(
            bundle.manifestlog.datastore,
            *bundledatastores)
        bundle.manifestlog.historystore = metadatastore.unionmetadatastore(
            bundle.manifestlog.historystore,
            *bundlehiststores)

    return bundle

@bundle2.parthandler(rebaseparttype, ('onto', 'newhead', 'obsmarkerversions'))
def bundle2rebase(op, part):
    '''unbundle a bundle2 containing a changegroup to rebase'''

    params = part.params

    bundlefile = None
    bundle = None
    markerdate = util.makedate()

    try: # guards bundlefile
        bundlefile = _makebundlefile(part)
        bundlepath = "bundle:%s+%s" % (op.repo.root, bundlefile)
        bundle = _createbundlerepo(op, bundlepath)

        prepushrebasehooks(op, params, bundle, bundlefile)

        op.repo.ui.setconfig('pushrebase', pushrebasemarker, True)

        bundlerepocache, preontocache = prefetchcaches(op, params, bundle)

        # Create a cache of rename sources while we don't have the lock.
        renamesrccache = {bundle[r].node(): _getrenamesrcs(op, bundle[r])
                          for r in bundle.revs('bundle()')}

        # Opening the transaction takes the lock, so do it after prepushrebase
        # and after we've fetched all the cache information we'll need.
        tr = op.gettransaction()
        hookargs = dict(tr.hookargs)

        # Recreate the bundle repo, since taking the lock in gettransaction()
        # may have caused it to become out of date.
        # (but grab a copy of the cache first)
        bundle.close()
        bundle = _createbundlerepo(op, bundlepath)

        prefillcaches(op, bundle, bundlerepocache)

        onto = getontotarget(op, params, bundle)

        revs, oldonto = _getrevs(op, bundle, onto, renamesrccache)

        op.repo.hook("prechangegroup", **hookargs)

        printpushmessage(op, revs, bundle)

        # Prepopulate the revlog _cache with the original onto's fulltext. This
        # means reading the new onto's manifest will likely have a much shorter
        # delta chain to traverse.
        if preontocache:
            op.repo.manifestlog._revlog._cache = preontocache
            onto.manifest()

        # Perform the rebase + commit to the main repo
        added, replacements = runrebase(op, revs, oldonto, onto)

        markers = _buildobsolete(replacements, bundle, op.repo, markerdate)
    finally:
        try:
            if bundlefile:
                os.unlink(bundlefile)
        except OSError as e:
            if e.errno != errno.ENOENT:
                raise
        if bundle:
            bundle.close()

    # Move public phase forward
    publishing = op.repo.ui.configbool('phases', 'publish', untrusted=True)
    if publishing:
        phasesmod.advanceboundary(op.repo, tr, phasesmod.public, [added[-1]])

    addfinalhooks(op, tr, hookargs, added)

    # Send new commits back to the client
    clientobsmarkerversions = [
        int(v) for v in params.get('obsmarkerversions', '').split('\0') if v]
    _addpushbackparts(op, replacements, markers, markerdate,
                      clientobsmarkerversions)

    for k in replacements.keys():
        replacements[hex(k)] = hex(replacements[k])
    op.records.add(rebaseparttype, replacements)

    return 1

def prepushrebasehooks(op, params, bundle, bundlefile):
    onto = params.get('onto')
    prelockonto = resolveonto(op.repo, onto or donotrebasemarker)
    prelockontonode = prelockonto.hex() if prelockonto else None

    # Allow running hooks on the new commits before we take the lock
    prelockrebaseargs = op.hookargs.copy()
    prelockrebaseargs['source'] = 'push'
    prelockrebaseargs['bundle2'] = '1'
    prelockrebaseargs['node'] = scmutil.revsingle(bundle,
                                                  'min(bundle())').hex()
    prelockrebaseargs['node_onto'] = prelockontonode
    if onto:
        prelockrebaseargs['onto'] = onto
    prelockrebaseargs['hook_bundlepath'] = bundlefile

    for path in op.records[treepackrecords]:
        if ':' in path:
            raise RuntimeError(_("tree pack path may not contain colon (%s)") %
                               path)
    prelockrebaseargs['hook_packpaths'] = ':'.join(op.records[treepackrecords])

    op.repo.hook("prepushrebase", throw=True, **prelockrebaseargs)

def prefetchcaches(op, params, bundle):
    bundlerepocache = {}
    # No need to cache trees from the bundle since they are already fast
    if not op.records[treepackrecords]:
        # We will need the bundle revs after the lock is taken, so let's
        # precache all the bundle rev manifests.
        bundlectxs = list(bundle.set('bundle()'))
        manifestcachesize = op.repo.ui.configint('format',
                                                 'manifestcachesize') or 10
        if len(bundlectxs) < manifestcachesize:
            for ctx in bundlectxs:
                bundlerepocache[ctx.manifestnode()] = ctx.manifestctx().read()

    preonto = resolveonto(bundle, params.get('onto', donotrebasemarker))
    preontocache = None
    if preonto:
        cache = bundle.manifestlog._revlog._cache
        if cache:
            cachenode, cacherev, cachetext = cache
            if cachenode == preonto.node():
                preontocache = cache
        if not preontocache:
            # TODO: skip if tree-only bundle revision
            cachenode = preonto.manifestnode()
            cacherev = bundle.manifestlog._revlog.rev(cachenode)
            cachetext = bundle.manifestlog[cachenode].read().text()
            preontocache = (cachenode, cacherev, cachetext)

    return bundlerepocache, preontocache

def prefillcaches(op, bundle, bundlerepocache):
    # Preload the caches with data we already have. We need to make copies
    # here so that original repo caches don't get tainted with bundle
    # specific data.
    newdirmancache = bundle.manifestlog._dirmancache
    for dir, dircache in op.repo.manifestlog._dirmancache.iteritems():
        for mfnode in dircache:
            mfctx = dircache[mfnode]
            newmfctx = manifest.manifestctx(bundle.manifestlog, mfnode)
            if (util.safehasattr(mfctx, '_data') and
                util.safehasattr(newmfctx, '_data')):
                    newmfctx._data = mfctx._data
                    newdirmancache[dir][mfnode] = newmfctx

    for mfnode, mfdict in bundlerepocache.iteritems():
        newmfctx = manifest.manifestctx(bundle.manifestlog, mfnode)
        newmfctx._data = mfdict
        newdirmancache[""][mfnode] = newmfctx

    newfulltextcache = op.repo.manifestlog._revlog._fulltextcache.copy()
    bundle.manifestlog._revlog._fulltextcache = newfulltextcache

def getontotarget(op, params, bundle):
    onto = resolveonto(op.repo, params.get('onto', donotrebasemarker))

    if not params['newhead']:
        if not op.repo.revs('%r and head()', params['onto']):
            raise error.Abort(_('rebase would create a new head on server'))

    if onto is None:
        maxcommonanc = list(bundle.set('max(parents(bundle()) - bundle())'))
        if not maxcommonanc:
            onto = op.repo[nullid]
        else:
            onto = maxcommonanc[0]
    return onto

def printpushmessage(op, revs, bundle):
    # Notify the user of what is being pushed
    plural = 's' if len(revs) > 1 else ''
    op.repo.ui.warn(_("pushing %s changeset%s:\n") % (len(revs), plural))
    maxoutput = 10
    for i in range(0, min(len(revs), maxoutput)):
        firstline = bundle[revs[i]].description().split('\n')[0][:50]
        op.repo.ui.warn(("    %s  %s\n") % (revs[i], firstline))

    if len(revs) > maxoutput + 1:
        op.repo.ui.warn(("    ...\n"))
        firstline = bundle[revs[-1]].description().split('\n')[0][:50]
        op.repo.ui.warn(("    %s  %s\n") % (revs[-1], firstline))

def runrebase(op, revs, oldonto, onto):
    mapping = {}
    replacements = {}
    added = []

    # Seed the mapping with oldonto->onto
    mapping[oldonto.node()] = onto.node()

    lastdestnode = None
    for rev in revs:
        newrev = _graft(op, rev, mapping, lastdestnode)

        new = op.repo[newrev]
        oldnode = rev.node()
        newnode = new.node()
        replacements[oldnode] = newnode
        mapping[oldnode] = newnode
        added.append(newnode)

        # Track which commit contains the original rebase destination
        # contents, so we can preserve the appropriate side's content during
        # merges.
        if not lastdestnode or oldnode == lastdestnode:
            lastdestnode = newnode

    return added, replacements

def addfinalhooks(op, tr, hookargs, added):
    hookargs['node'] = tr.hookargs['node'] = hex(added[0])
    hookargs['node_last'] = hex(added[-1])

    p = lambda: tr.writepending() and op.repo.root or ""
    op.repo.hook("pretxnchangegroup", throw=True, pending=p, **hookargs)

    def runhooks():
        args = hookargs.copy()
        op.repo.hook("changegroup", **hookargs)
        args.pop('node_last')
        for n in added:
            args = hookargs.copy()
            args['node'] = hex(n)
            op.repo.hook("incoming", **args)

    tr.addpostclose('serverrebase-cg-hooks',
                    lambda tr: op.repo._afterlock(runhooks))

def bundle2pushkey(orig, op, part):
    # Merges many dicts into one. First it converts them to list of pairs,
    # then concatenates them (using sum), and then creates a diff out of them.
    replacements = dict(sum([record.items()
                             for record
                             in op.records[rebaseparttype]],
                            []))

    namespace = pushkey.decode(part.params['namespace'])
    if namespace == 'phases':
        key = pushkey.decode(part.params['key'])
        part.params['key'] = pushkey.encode(replacements.get(key, key))
    if namespace == 'bookmarks':
        new = pushkey.decode(part.params['new'])
        part.params['new'] = pushkey.encode(replacements.get(new, new))
        serverbin = op.repo._bookmarks.get(part.params['key'])
        clienthex = pushkey.decode(part.params['old'])

        if serverbin and clienthex:
            cl = op.repo.changelog
            revserver = cl.rev(serverbin)
            revclient = cl.rev(bin(clienthex))
            if revclient in cl.ancestors([revserver]):
                # if the client's bookmark origin is an lagging behind the
                # server's location for that bookmark (usual for pushrebase)
                # then update the old location to match the real location
                #
                # TODO: We would prefer to only do this for pushrebase pushes
                # but that isn't straightforward so we just do it always here.
                # This forbids moving bookmarks backwards from clients.
                part.params['old'] = pushkey.encode(hex(serverbin))

    return orig(op, part)

def bundle2phaseheads(orig, op, part):
    # Merges many dicts into one. First it converts them to list of pairs,
    # then concatenates them (using sum), and then creates a diff out of them.
    replacements = dict(sum([record.items()
                             for record
                             in op.records[rebaseparttype]],
                            []))

    decodedphases = phasesmod.binarydecode(part)

    replacedphases = []
    for phasetype in decodedphases:
        replacedphases.append(
            [replacements.get(node, node) for node in phasetype])
    # Since we've just read the bundle part, then `orig()` won't be able to
    # read it again. Let's replace payload stream with new stream of replaced
    # nodes.
    part._payloadstream = util.chunkbuffer(
        [phasesmod.binaryencode(replacedphases)])
    return orig(op, part)
