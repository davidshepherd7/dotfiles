from __future__ import absolute_import

import os
from collections import defaultdict
from hgext3rd.extutil import runshellcommand
from mercurial import (
    error,
    extensions,
    mdiff,
    policy,
    scmutil,
    util,
)
from mercurial.node import nullid
from mercurial.i18n import _
from . import (
    constants,
    contentstore,
    datapack,
    historypack,
    metadatastore,
    shallowutil,
)
import time

osutil = policy.importmod(r'osutil')

def backgroundrepack(repo, incremental=True):
    cmd = util.hgcmd() + ['-R', repo.origroot, 'repack']
    msg = _("(running background repack)\n")
    if incremental:
        cmd.append('--incremental')
        msg = _("(running background incremental repack)\n")
    cmd = ' '.join(map(util.shellquote, cmd))

    repo.ui.warn(msg)
    runshellcommand(cmd, os.environ)

def fullrepack(repo):
    if util.safehasattr(repo, 'shareddatastores'):
        datasource = contentstore.unioncontentstore(*repo.shareddatastores)
        historysource = metadatastore.unionmetadatastore(
            *repo.sharedhistorystores,
            allowincomplete=True)

        packpath = shallowutil.getcachepackpath(
            repo,
            constants.FILEPACK_CATEGORY)
        _runrepack(repo, datasource, historysource, packpath,
                   constants.FILEPACK_CATEGORY)

    if repo.ui.configbool('treemanifest', 'server'):
        treemfmod = extensions.find('treemanifest')
        treemfmod.serverrepack(repo)
    elif util.safehasattr(repo.manifestlog, 'datastore'):
        localdata, shareddata = _getmanifeststores(repo)
        lpackpath, ldstores, lhstores = localdata
        spackpath, sdstores, shstores = shareddata

        # Repack the shared manifest store
        datasource = contentstore.unioncontentstore(*sdstores)
        historysource = metadatastore.unionmetadatastore(
                        *shstores,
                        allowincomplete=True)
        _runrepack(repo, datasource, historysource, spackpath,
                   constants.TREEPACK_CATEGORY)

        # Repack the local manifest store
        datasource = contentstore.unioncontentstore(
                        *ldstores,
                        allowincomplete=True)
        historysource = metadatastore.unionmetadatastore(
                        *lhstores,
                        allowincomplete=True)
        _runrepack(repo, datasource, historysource, lpackpath,
                   constants.TREEPACK_CATEGORY)


def incrementalrepack(repo):
    """This repacks the repo by looking at the distribution of pack files in the
    repo and performing the most minimal repack to keep the repo in good shape.
    """
    if util.safehasattr(repo, 'shareddatastores'):
        packpath = shallowutil.getcachepackpath(
            repo,
            constants.FILEPACK_CATEGORY)
        _incrementalrepack(repo,
                           repo.shareddatastores,
                           repo.sharedhistorystores,
                           packpath,
                           constants.FILEPACK_CATEGORY)

    if repo.ui.configbool('treemanifest', 'server'):
        treemfmod = extensions.find('treemanifest')
        treemfmod.serverrepack(repo, incremental=True)
    elif util.safehasattr(repo.manifestlog, 'datastore'):
        localdata, shareddata = _getmanifeststores(repo)
        lpackpath, ldstores, lhstores = localdata
        spackpath, sdstores, shstores = shareddata

        # Repack the shared manifest store
        _incrementalrepack(repo,
                           sdstores,
                           shstores,
                           spackpath,
                           constants.TREEPACK_CATEGORY)

        # Repack the local manifest store
        _incrementalrepack(repo,
                           ldstores,
                           lhstores,
                           lpackpath,
                           constants.TREEPACK_CATEGORY,
                           allowincompletedata=True)

def _getmanifeststores(repo):
    shareddatastores = repo.manifestlog.shareddatastores
    localdatastores = repo.manifestlog.localdatastores
    sharedhistorystores = repo.manifestlog.sharedhistorystores
    localhistorystores = repo.manifestlog.localhistorystores

    sharedpackpath = shallowutil.getcachepackpath(repo,
                                            constants.TREEPACK_CATEGORY)
    localpackpath = shallowutil.getlocalpackpath(repo.svfs.vfs.base,
                                            constants.TREEPACK_CATEGORY)

    # The native stores don't support repacking yet, so fall back to the
    # python versions.
    if repo.ui.configbool("treemanifest", "usecunionstore"):
        usecdatapack = repo.ui.configbool("remotefilelog", "fastdatapack")
        shareddatastores = [datapack.datapackstore(repo.ui, sharedpackpath,
                                      usecdatapack=usecdatapack)]
        localdatastores = [datapack.datapackstore(repo.ui, localpackpath,
                                     usecdatapack=usecdatapack)]

    return ((localpackpath, localdatastores, localhistorystores),
            (sharedpackpath, shareddatastores, sharedhistorystores))

def _incrementalrepack(repo, datastore, historystore, packpath, category,
        allowincompletedata=False):
    shallowutil.mkstickygroupdir(repo.ui, packpath)

    files = osutil.listdir(packpath, stat=True)

    datapacks = _computeincrementaldatapack(repo.ui, files)
    fullpaths = list(os.path.join(packpath, p) for p in datapacks)
    datapacks = list(datapack.datapack(p) for p in fullpaths)
    datapacks.extend(s for s in datastore
                     if not isinstance(s, datapack.datapackstore))

    historypacks = _computeincrementalhistorypack(repo.ui, files)
    fullpaths = list(os.path.join(packpath, p) for p in historypacks)
    historypacks = list(historypack.historypack(p) for p in fullpaths)
    historypacks.extend(s for s in historystore
                        if not isinstance(s, historypack.historypackstore))

    datasource = contentstore.unioncontentstore(
        *datapacks,
        allowincomplete=allowincompletedata)
    historysource = metadatastore.unionmetadatastore(*historypacks,
                                                     allowincomplete=True)

    _runrepack(repo, datasource, historysource, packpath, category)

def _computeincrementaldatapack(ui, files):
    """Given a set of pack files and a set of generation size limits, this
    function computes the list of files that should be packed as part of an
    incremental repack.

    It tries to strike a balance between keeping incremental repacks cheap (i.e.
    packing small things when possible, and rolling the packs up to the big ones
    over time).
    """
    generations = ui.configlist("remotefilelog", "data.generations",
                                ['1GB', '100MB', '1MB'])
    generations = list(sorted((util.sizetoint(s) for s in generations),
                                reverse=True))
    generations.append(0)

    gencountlimit = ui.configint('remotefilelog', 'data.gencountlimit', 2)
    repacksizelimit = ui.configbytes('remotefilelog', 'data.repacksizelimit',
                                     '100MB')

    return _computeincrementalpack(ui, files, generations, datapack.PACKSUFFIX,
            datapack.INDEXSUFFIX, gencountlimit, repacksizelimit)

def _computeincrementalhistorypack(ui, files):
    generations = ui.configlist("remotefilelog", "history.generations",
                                ['100MB'])
    generations = list(sorted((util.sizetoint(s) for s in generations),
                                reverse=True))
    generations.append(0)

    gencountlimit = ui.configint('remotefilelog', 'history.gencountlimit', 2)
    repacksizelimit = ui.configbytes('remotefilelog', 'history.repacksizelimit',
                                     '100MB')

    return _computeincrementalpack(ui, files, generations,
            historypack.PACKSUFFIX, historypack.INDEXSUFFIX, gencountlimit,
            repacksizelimit)

def _computeincrementalpack(ui, files, limits, packsuffix, indexsuffix,
                            gencountlimit, repacksizelimit):
    # Group the packs by generation (i.e. by size)
    generations = []
    for i in xrange(len(limits)):
        generations.append([])
    sizes = {}
    fileset = set(fn for fn, mode, stat in files)
    for filename, mode, stat in files:
        if not filename.endswith(packsuffix):
            continue

        prefix = filename[:-len(packsuffix)]

        # Don't process a pack if it doesn't have an index.
        if (prefix + indexsuffix) not in fileset:
            continue

        size = stat.st_size
        sizes[prefix] = size
        for i, limit in enumerate(limits):
            if size > limit:
                generations[i].append(prefix)
                break

    # Find the largest generation with more than 2 packs and repack it.
    for i, limit in enumerate(limits):
        if len(generations[i]) > gencountlimit:
            # Try to repack 3 things at once. This means if we run an
            # incremental repack right after we add a new pack file, we'll still
            # decrease the total number of pack files.
            count = 3
            if sum(sizes[n] for n in generations[i]) < repacksizelimit:
                count = len(generations[i])
            return sorted(generations[i], key=lambda x: sizes[x])[:count]

    # If no generation has more than 2 packs, repack as many as fit into the
    # limit
    small = set().union(*generations[1:])
    if len(small) > 1:
        total = 0
        packs = []
        for pack in sorted(small, key=lambda x: sizes[x]):
            size = sizes[pack]
            if total + size < repacksizelimit:
                packs.append(pack)
                total += size
            else:
                break

        if len(packs) > 1:
            return packs

    # If there aren't small ones to repack, repack the two largest ones.
    if len(generations[0]) > 1:
        return generations[0]

    return []

def _runrepack(repo, data, history, packpath, category):
    shallowutil.mkstickygroupdir(repo.ui, packpath)

    def isold(repo, filename, node):
        """Check if the file node is older than a limit.
        Unless a limit is specified in the config the default limit is taken.
        """
        filectx = repo.filectx(filename, fileid=node)
        filetime = repo[filectx.linkrev()].date()

        # Currently default TTL limit is 30 days
        defaultlimit = 60 * 60 * 24 * 30
        ttl = repo.ui.configint('remotefilelog', 'nodettl', defaultlimit)

        limit = time.time() - ttl
        return filetime[0] < limit

    packer = repacker(repo, data, history, category, isold)

    # internal config: remotefilelog.datapackversion
    dv = repo.ui.configint('remotefilelog', 'datapackversion', 0)

    with datapack.mutabledatapack(repo.ui, packpath, version=dv) as dpack:
        with historypack.mutablehistorypack(repo.ui, packpath) as hpack:
            try:
                packer.run(dpack, hpack)
            except error.LockHeld:
                raise error.Abort(_("skipping repack - another repack is "
                                    "already running"))

class repacker(object):
    """Class for orchestrating the repack of data and history information into a
    new format.
    """
    def __init__(self, repo, data, history, category, isold=None):
        self.repo = repo
        self.data = data
        self.history = history
        self.unit = constants.getunits(category)
        self.garbagecollect = repo.ui.configbool('remotefilelog', 'gcrepack')
        if self.garbagecollect:
            if not isold:
                raise ValueError("Function 'isold' is not properly specified")
            self.keepkeys = self._gckeepset()
            self.isold = isold

    def _gckeepset(self):
        """Computes a keepset which is not garbage collected.
        """
        repo = self.repo
        revs = ['.', 'draft()', 'parents(draft())', '(heads(all()) & date(-7))']

        # If pullprefetch and bgprefetchrevs are specified include them as well
        # since we don't want to prefetch and immediately garbage collect them
        prefetchrevs = repo.ui.config('remotefilelog', 'pullprefetch', None)
        if prefetchrevs:
            revs.append('(%s)' % prefetchrevs)
        prefetchrevs = repo.ui.config('remotefilelog', 'bgprefetchrevs', None)
        if prefetchrevs:
            revs.append('(%s)' % prefetchrevs)

        keep = scmutil.revrange(repo, ['+'.join(revs)])
        keepkeys = set()
        for r in keep:
            m = repo[r].manifest()
            keepkeys.update(m.iteritems())

        return keepkeys

    def run(self, targetdata, targethistory):
        ledger = repackledger()

        with self.repo._lock(self.repo.svfs, "repacklock", False, None,
                             None, _('repacking %s') % self.repo.origroot):
            self.repo.hook('prerepack')

            # Populate ledger from source
            self.data.markledger(ledger)
            self.history.markledger(ledger)

            # Run repack
            self.repackdata(ledger, targetdata)
            self.repackhistory(ledger, targethistory)

            # Call cleanup on each source
            for source in ledger.sources:
                source.cleanup(ledger)

    def repackdata(self, ledger, target):
        ui = self.repo.ui
        maxchainlen = ui.configint('packs', 'maxchainlen', 1000)

        byfile = {}
        for entry in ledger.entries.itervalues():
            if entry.datasource:
                byfile.setdefault(entry.filename, {})[entry.node] = entry

        count = 0
        for filename, entries in sorted(byfile.iteritems()):
            ui.progress(_("repacking data"), count, unit=self.unit,
                        total=len(byfile))

            ancestors = {}
            nodes = list(node for node in entries.iterkeys())
            nohistory = []
            for i, node in enumerate(nodes):
                if node in ancestors:
                    continue
                ui.progress(_("building history"), i, unit='nodes',
                            total=len(nodes))
                try:
                    ancestors.update(self.history.getancestors(filename, node,
                                                               known=ancestors))
                except KeyError:
                    # Since we're packing data entries, we may not have the
                    # corresponding history entries for them. It's not a big
                    # deal, but the entries won't be delta'd perfectly.
                    nohistory.append(node)
            ui.progress(_("building history"), None)

            # Order the nodes children first, so we can produce reverse deltas
            orderednodes = list(reversed(self._toposort(ancestors)))
            orderednodes.extend(sorted(nohistory))

            # Compute deltas and write to the pack
            deltabases = defaultdict(lambda: (nullid, 0))
            nodes = set(nodes)
            for i, node in enumerate(orderednodes):
                # orderednodes is all ancestors, but we only want to serialize
                # the files we have.
                if node not in nodes:
                    continue

                if self.garbagecollect:
                    # If the node is old and is not in the keepset
                    # we skip it and mark as garbage collected
                    if ((filename, node) not in self.keepkeys and
                                        self.isold(self.repo, filename, node)):
                        entries[node].gced = True
                        continue

                ui.progress(_("processing nodes"), i, unit='nodes',
                            total=len(orderednodes))
                # Find delta base
                # TODO: allow delta'ing against most recent descendant instead
                # of immediate child
                deltabase, chainlen = deltabases[node]

                # Use available ancestor information to inform our delta choices
                ancestorinfo = ancestors.get(node)
                if ancestorinfo:
                    p1, p2, linknode, copyfrom = ancestorinfo

                    # The presence of copyfrom means we're at a point where the
                    # file was copied from elsewhere. So don't attempt to do any
                    # deltas with the other file.
                    if copyfrom:
                        p1 = nullid

                    if chainlen < maxchainlen:
                        # Record this child as the delta base for its parents.
                        # This may be non optimal, since the parents may have
                        # many children, and this will only choose the last one.
                        # TODO: record all children and try all deltas to find
                        # best
                        if p1 != nullid:
                            deltabases[p1] = (node, chainlen + 1)
                        if p2 != nullid:
                            deltabases[p2] = (node, chainlen + 1)

                # Compute delta
                # TODO: Optimize the deltachain fetching. Since we're
                # iterating over the different version of the file, we may
                # be fetching the same deltachain over and over again.
                # TODO: reuse existing deltas if it matches our deltabase
                if deltabase != nullid:
                    deltabasetext = self.data.get(filename, deltabase)
                    original = self.data.get(filename, node)
                    delta = mdiff.textdiff(deltabasetext, original)
                else:
                    delta = self.data.get(filename, node)

                # TODO: don't use the delta if it's larger than the fulltext
                # TODO: don't use the delta if the chain is already long
                meta = self.data.getmeta(filename, node)
                target.add(filename, node, deltabase, delta, meta)

                entries[node].datarepacked = True

            ui.progress(_("processing nodes"), None)
            count += 1

        ui.progress(_("repacking data"), None)
        target.close(ledger=ledger)

    def repackhistory(self, ledger, target):
        ui = self.repo.ui

        byfile = {}
        for entry in ledger.entries.itervalues():
            if entry.historysource:
                byfile.setdefault(entry.filename, {})[entry.node] = entry

        count = 0
        for filename, entries in sorted(byfile.iteritems()):
            ancestors = {}
            nodes = list(node for node in entries.iterkeys())

            for node in nodes:
                if node in ancestors:
                    continue
                ancestors.update(self.history.getancestors(filename, node,
                                                           known=ancestors))

            # Order the nodes children first
            orderednodes = reversed(self._toposort(ancestors))

            # Write to the pack
            dontprocess = set()
            for node in orderednodes:
                p1, p2, linknode, copyfrom = ancestors[node]

                # If the node is marked dontprocess, but it's also in the
                # explicit entries set, that means the node exists both in this
                # file and in another file that was copied to this file.
                # Usually this happens if the file was copied to another file,
                # then the copy was deleted, then reintroduced without copy
                # metadata. The original add and the new add have the same hash
                # since the content is identical and the parents are null.
                if node in dontprocess and node not in entries:
                    # If copyfrom == filename, it means the copy history
                    # went to come other file, then came back to this one, so we
                    # should continue processing it.
                    if p1 != nullid and copyfrom != filename:
                        dontprocess.add(p1)
                    if p2 != nullid:
                        dontprocess.add(p2)
                    continue

                if copyfrom:
                    dontprocess.add(p1)

                target.add(filename, node, p1, p2, linknode, copyfrom)

                if node in entries:
                    entries[node].historyrepacked = True

            count += 1
            ui.progress(_("repacking history"), count, unit=self.unit,
                        total=len(byfile))

        ui.progress(_("repacking history"), None)
        target.close(ledger=ledger)

    def _toposort(self, ancestors):
        def parentfunc(node):
            p1, p2, linknode, copyfrom = ancestors[node]
            parents = []
            if p1 != nullid:
                parents.append(p1)
            if p2 != nullid:
                parents.append(p2)
            return parents

        sortednodes = shallowutil.sortnodes(ancestors.keys(), parentfunc)
        return sortednodes

class repackledger(object):
    """Storage for all the bookkeeping that happens during a repack. It contains
    the list of revisions being repacked, what happened to each revision, and
    which source store contained which revision originally (for later cleanup).
    """
    def __init__(self):
        self.entries = {}
        self.sources = {}
        self.created = set()

    def markdataentry(self, source, filename, node):
        """Mark the given filename+node revision as having a data rev in the
        given source.
        """
        entry = self._getorcreateentry(filename, node)
        entry.datasource = True
        entries = self.sources.get(source)
        if not entries:
            entries = set()
            self.sources[source] = entries
        entries.add(entry)

    def markhistoryentry(self, source, filename, node):
        """Mark the given filename+node revision as having a history rev in the
        given source.
        """
        entry = self._getorcreateentry(filename, node)
        entry.historysource = True
        entries = self.sources.get(source)
        if not entries:
            entries = set()
            self.sources[source] = entries
        entries.add(entry)

    def _getorcreateentry(self, filename, node):
        key = (filename, node)
        value = self.entries.get(key)
        if not value:
            value = repackentry(filename, node)
            self.entries[key] = value

        return value

    def addcreated(self, value):
        self.created.add(value)

class repackentry(object):
    """Simple class representing a single revision entry in the repackledger.
    """
    __slots__ = ['filename', 'node', 'datasource', 'historysource',
                 'datarepacked', 'historyrepacked', 'gced']
    def __init__(self, filename, node):
        self.filename = filename
        self.node = node
        # If the revision has a data entry in the source
        self.datasource = False
        # If the revision has a history entry in the source
        self.historysource = False
        # If the revision's data entry was repacked into the repack target
        self.datarepacked = False
        # If the revision's history entry was repacked into the repack target
        self.historyrepacked = False
        # If garbage collected
        self.gced = False
