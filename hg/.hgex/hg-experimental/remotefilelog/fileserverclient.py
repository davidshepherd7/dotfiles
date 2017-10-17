# fileserverclient.py - client for communicating with the cache process
#
# Copyright 2013 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from mercurial.i18n import _
from mercurial.node import hex, bin, nullid
from mercurial import util, sshpeer, error, util, wireproto, httppeer
import hashlib, os, time, io, struct
import itertools

from . import (
    connectionpool,
    constants,
    shallowutil,
    wirepack,
)
from .contentstore import unioncontentstore
from .metadatastore import unionmetadatastore
from lz4wrapper import lz4decompress

# Statistics for debugging
fetchcost = 0
fetches = 0
fetched = 0
fetchmisses = 0

_downloading = _('downloading')

def getcachekey(reponame, file, id):
    pathhash = hashlib.sha1(file).hexdigest()
    return os.path.join(reponame, pathhash[:2], pathhash[2:], id)

def getlocalkey(file, id):
    pathhash = hashlib.sha1(file).hexdigest()
    return os.path.join(pathhash, id)

def peersetup(ui, peer):
    class remotefilepeer(peer.__class__):
        @wireproto.batchable
        def getfile(self, file, node):
            if not self.capable('getfile'):
                raise error.Abort(
                    'configured remotefile server does not support getfile')
            f = wireproto.future()
            yield {'file': file, 'node': node}, f
            code, data = f.value.split('\0', 1)
            if int(code):
                raise error.LookupError(file, node, data)
            yield data

        @wireproto.batchable
        def getflogheads(self, path):
            if not self.capable('getflogheads'):
                raise error.Abort('configured remotefile server does not '
                                  'support getflogheads')
            f = wireproto.future()
            yield {'path': path}, f
            heads = f.value.split('\n') if f.value else []
            yield heads

        def _updatecallstreamopts(self, command, opts):
            if command != 'getbundle':
                return
            if 'remotefilelog' not in self._capabilities():
                return
            if not util.safehasattr(self, '_localrepo'):
                return
            if constants.REQUIREMENT not in self._localrepo.requirements:
                return

            bundlecaps = opts.get('bundlecaps')
            if bundlecaps:
                bundlecaps = [bundlecaps]
            else:
                bundlecaps = []

            # shallow, includepattern, and excludepattern are a hacky way of
            # carrying over data from the local repo to this getbundle
            # command. We need to do it this way because bundle1 getbundle
            # doesn't provide any other place we can hook in to manipulate
            # getbundle args before it goes across the wire. Once we get rid
            # of bundle1, we can use bundle2's _pullbundle2extraprepare to
            # do this more cleanly.
            bundlecaps.append('remotefilelog')
            if self._localrepo.includepattern:
                patterns = '\0'.join(self._localrepo.includepattern)
                includecap = "includepattern=" + patterns
                bundlecaps.append(includecap)
            if self._localrepo.excludepattern:
                patterns = '\0'.join(self._localrepo.excludepattern)
                excludecap = "excludepattern=" + patterns
                bundlecaps.append(excludecap)
            opts['bundlecaps'] = ','.join(bundlecaps)

        def _callstream(self, command, **opts):
            self._updatecallstreamopts(command, opts)
            return super(remotefilepeer, self)._callstream(command, **opts)

    peer.__class__ = remotefilepeer

class cacheconnection(object):
    """The connection for communicating with the remote cache. Performs
    gets and sets by communicating with an external process that has the
    cache-specific implementation.
    """
    def __init__(self):
        self.pipeo = self.pipei = self.pipee = None
        self.subprocess = None
        self.connected = False

    def connect(self, cachecommand):
        if self.pipeo:
            raise error.Abort(_("cache connection already open"))
        self.pipei, self.pipeo, self.pipee, self.subprocess = \
            util.popen4(cachecommand)
        self.connected = True

    def close(self):
        def tryclose(pipe):
            try:
                pipe.close()
            except Exception:
                pass
        if self.connected:
            try:
                self.pipei.write("exit\n")
            except Exception:
                pass
            tryclose(self.pipei)
            self.pipei = None
            tryclose(self.pipeo)
            self.pipeo = None
            tryclose(self.pipee)
            self.pipee = None
            try:
                # Wait for process to terminate, making sure to avoid deadlock.
                # See https://docs.python.org/2/library/subprocess.html for
                # warnings about wait() and deadlocking.
                self.subprocess.communicate()
            except Exception:
                pass
            self.subprocess = None
        self.connected = False

    def request(self, request, flush=True):
        if self.connected:
            try:
                self.pipei.write(request)
                if flush:
                    self.pipei.flush()
            except IOError:
                self.close()

    def receiveline(self):
        if not self.connected:
            return None
        try:
            result = self.pipeo.readline()[:-1]
            if not result:
                self.close()
        except IOError:
            self.close()

        return result

def _getfilesbatch(
        remote, receivemissing, progresstick, missed, idmap, batchsize):
    # Over http(s), iterbatch is a streamy method and we can start
    # looking at results early. This means we send one (potentially
    # large) request, but then we show nice progress as we process
    # file results, rather than showing chunks of $batchsize in
    # progress.
    #
    # Over ssh, iterbatch isn't streamy because batch() wasn't
    # explicitly designed as a streaming method. In the future we
    # should probably introduce a streambatch() method upstream and
    # use that for this.
    if (getattr(remote, 'iterbatch', False) and remote.capable('httppostargs')
        and isinstance(remote, httppeer.httppeer)):
        b = remote.iterbatch()
        for m in missed:
            file_ = idmap[m]
            node = m[-40:]
            b.getfile(file_, node)
        b.submit()
        for m, r in itertools.izip(missed, b.results()):
            file_ = idmap[m]
            node = m[-40:]
            receivemissing(io.BytesIO('%d\n%s' % (len(r), r)), file_, node)
            progresstick()
        return
    while missed:
        chunk, missed = missed[:batchsize], missed[batchsize:]
        b = remote.batch()
        futures = {}
        for m in chunk:
            file_ = idmap[m]
            node = m[-40:]
            futures[m] = b.getfile(file_, node)
        b.submit()
        for m in chunk:
            v = futures[m].value
            file_ = idmap[m]
            node = m[-40:]
            receivemissing(io.BytesIO('%d\n%s' % (len(v), v)), file_, node)
            progresstick()

def _getfiles(
    remote, receivemissing, progresstick, missed, idmap, step):
    remote._callstream("getfiles")
    i = 0
    while i < len(missed):
        # issue a batch of requests
        start = i
        end = min(len(missed), start + step)
        i = end
        for missingid in missed[start:end]:
            # issue new request
            versionid = missingid[-40:]
            file = idmap[missingid]
            sshrequest = "%s%s\n" % (versionid, file)
            remote.pipeo.write(sshrequest)
        remote.pipeo.flush()

        # receive batch results
        for missingid in missed[start:end]:
            versionid = missingid[-40:]
            file = idmap[missingid]
            receivemissing(remote.pipei, file, versionid)
            progresstick()

    # End the command
    remote.pipeo.write('\n')
    remote.pipeo.flush()

class fileserverclient(object):
    """A client for requesting files from the remote file server.
    """
    def __init__(self, repo):
        ui = repo.ui
        self.repo = repo
        self.ui = ui
        self.cacheprocess = ui.config("remotefilelog", "cacheprocess")
        if self.cacheprocess:
            self.cacheprocess = util.expandpath(self.cacheprocess)


        # This option causes remotefilelog to pass the full file path to the
        # cacheprocess instead of a hashed key.
        self.cacheprocesspasspath = ui.configbool(
            "remotefilelog", "cacheprocess.includepath")

        self.debugoutput = ui.configbool("remotefilelog", "debug")

        self.remotecache = cacheconnection()
        self.connpool = connectionpool.connectionpool(repo)

    def setstore(self, datastore, historystore, writedata, writehistory):
        self.datastore = datastore
        self.historystore = historystore
        self.writedata = writedata
        self.writehistory = writehistory

    def _connect(self):
        return self.connpool.get(self.repo.fallbackpath)

    def request(self, fileids):
        """Takes a list of filename/node pairs and fetches them from the
        server. Files are stored in the local cache.
        A list of nodes that the server couldn't find is returned.
        If the connection fails, an exception is raised.
        """
        if not self.remotecache.connected:
            self.connect()
        cache = self.remotecache
        writedata = self.writedata

        if self.ui.configbool('remotefilelog', 'fetchpacks'):
            self.requestpack(fileids)
            return

        repo = self.repo
        count = len(fileids)
        request = "get\n%d\n" % count
        idmap = {}
        reponame = repo.name
        for file, id in fileids:
            fullid = getcachekey(reponame, file, id)
            if self.cacheprocesspasspath:
                request += file + '\0'
            request += fullid + "\n"
            idmap[fullid] = file

        cache.request(request)

        total = count
        self.ui.progress(_downloading, 0, total=count)

        missed = []
        count = 0
        while True:
            missingid = cache.receiveline()
            if not missingid:
                missedset = set(missed)
                for missingid in idmap.iterkeys():
                    if not missingid in missedset:
                        missed.append(missingid)
                self.ui.warn(_("warning: cache connection closed early - " +
                    "falling back to server\n"))
                break
            if missingid == "0":
                break
            if missingid.startswith("_hits_"):
                # receive progress reports
                parts = missingid.split("_")
                count += int(parts[2])
                self.ui.progress(_downloading, count, total=total)
                continue

            missed.append(missingid)

        global fetchmisses
        fetchmisses += len(missed)

        count = [total - len(missed)]
        fromcache = count[0]
        self.ui.progress(_downloading, count[0], total=total)
        self.ui.log("remotefilelog", "remote cache hit rate is %r of %r ",
                    count[0], total, hit=count[0], total=total)

        oldumask = os.umask(0o002)
        try:
            # receive cache misses from master
            if missed:
                def progresstick():
                    count[0] += 1
                    self.ui.progress(_downloading, count[0], total=total)
                # When verbose is true, sshpeer prints 'running ssh...'
                # to stdout, which can interfere with some command
                # outputs
                verbose = self.ui.verbose
                self.ui.verbose = False
                try:
                    with self._connect() as conn:
                        remote = conn.peer
                        # TODO: deduplicate this with the constant in
                        #       shallowrepo
                        if remote.capable("remotefilelog"):
                            if not isinstance(remote, sshpeer.sshpeer):
                                raise error.Abort('remotefilelog requires ssh '
                                                  'servers')
                            step = self.ui.configint('remotefilelog',
                                                     'getfilesstep', 10000)
                            _getfiles(remote, self.receivemissing, progresstick,
                                      missed, idmap, step)
                        elif remote.capable("getfile"):
                            if remote.capable('batch'):
                                batchdefault = 100
                            else:
                                batchdefault = 10
                            batchsize = self.ui.configint(
                                'remotefilelog', 'batchsize', batchdefault)
                            _getfilesbatch(
                                remote, self.receivemissing, progresstick,
                                missed, idmap, batchsize)
                        else:
                            raise error.Abort("configured remotefilelog server"
                                             " does not support remotefilelog")

                    self.ui.log("remotefilefetchlog",
                                "Success",
                                fetched_files = count[0] - fromcache,
                                total_to_fetch = total - fromcache)
                except Exception:
                    self.ui.log("remotefilefetchlog",
                                "Fail",
                                fetched_files = count[0] - fromcache,
                                total_to_fetch = total - fromcache)
                    raise
                finally:
                    self.ui.verbose = verbose
                # send to memcache
                count[0] = len(missed)
                request = "set\n%d\n%s\n" % (count[0], "\n".join(missed))
                cache.request(request)

            self.ui.progress(_downloading, None)

            # mark ourselves as a user of this cache
            writedata.markrepo(self.repo.path)
        finally:
            os.umask(oldumask)

        return

    def receivemissing(self, pipe, filename, node):
        line = pipe.readline()[:-1]
        if not line:
            raise error.ResponseError(_("error downloading file contents:"),
                                      _("connection closed early"))
        size = int(line)
        data = pipe.read(size)
        if len(data) != size:
            raise error.ResponseError(_("error downloading file contents:"),
                                      _("only received %s of %s bytes")
                                      % (len(data), size))

        self.writedata.addremotefilelognode(filename, bin(node),
                                             lz4decompress(data))

    def requestpack(self, fileids):
        """Requests the given file revisions from the server in a pack format.

        See `remotefilelogserver.getpack` for the file format.
        """
        try:
            with self._connect() as conn:
                total = len(fileids)
                rcvd = 0

                remote = conn.peer
                remote._callstream("getpackv1")

                self._sendpackrequest(remote, fileids)

                packpath = shallowutil.getcachepackpath(
                    self.repo, constants.FILEPACK_CATEGORY)
                receiveddata, receivedhistory = wirepack.receivepack(
                    self.repo.ui, remote.pipei, packpath)
                rcvd = len(receiveddata)

            self.ui.log("remotefilefetchlog",
                        "Success(pack)" if (rcvd==total) else "Fail(pack)",
                        fetched_files = rcvd,
                        total_to_fetch = total)
        except Exception:
            self.ui.log("remotefilefetchlog",
                        "Fail(pack)",
                        fetched_files = rcvd,
                        total_to_fetch = total)
            raise

    def _sendpackrequest(self, remote, fileids):
        """Formats and writes the given fileids to the remote as part of a
        getpackv1 call.
        """
        # Sort the requests by name, so we receive requests in batches by name
        grouped = {}
        for filename, node in fileids:
            grouped.setdefault(filename, set()).add(node)

        # Issue request
        for filename, nodes in grouped.iteritems():
            filenamelen = struct.pack(constants.FILENAMESTRUCT, len(filename))
            countlen = struct.pack(constants.PACKREQUESTCOUNTSTRUCT, len(nodes))
            rawnodes = ''.join(bin(n) for n in nodes)

            remote.pipeo.write('%s%s%s%s' % (filenamelen, filename, countlen,
                                             rawnodes))
            remote.pipeo.flush()
        remote.pipeo.write(struct.pack(constants.FILENAMESTRUCT, 0))
        remote.pipeo.flush()

    def connect(self):
        if self.cacheprocess:
            cmd = "%s %s" % (self.cacheprocess, self.writedata._path)
            self.remotecache.connect(cmd)
        else:
            # If no cache process is specified, we fake one that always
            # returns cache misses.  This enables tests to run easily
            # and may eventually allow us to be a drop in replacement
            # for the largefiles extension.
            class simplecache(object):
                def __init__(self):
                    self.missingids = []
                    self.connected = True

                def close(self):
                    pass

                def request(self, value, flush=True):
                    lines = value.split("\n")
                    if lines[0] != "get":
                        return
                    self.missingids = lines[2:-1]
                    self.missingids.append('0')

                def receiveline(self):
                    if len(self.missingids) > 0:
                        return self.missingids.pop(0)
                    return None

            self.remotecache = simplecache()

    def close(self):
        if fetches:
            msg = ("%s files fetched over %d fetches - " +
                   "(%d misses, %0.2f%% hit ratio) over %0.2fs\n") % (
                       fetched,
                       fetches,
                       fetchmisses,
                       float(fetched - fetchmisses) / float(fetched) * 100.0,
                       fetchcost)
            if self.debugoutput:
                self.ui.warn(msg)
            self.ui.log("remotefilelog.prefetch", msg.replace("%", "%%"),
                remotefilelogfetched=fetched,
                remotefilelogfetches=fetches,
                remotefilelogfetchmisses=fetchmisses,
                remotefilelogfetchtime=fetchcost * 1000)

        if self.remotecache.connected:
            self.remotecache.close()

        self.connpool.close()

    def prefetch(self, fileids, force=False, fetchdata=True,
                 fetchhistory=False):
        """downloads the given file versions to the cache
        """
        repo = self.repo
        idstocheck = []
        for file, id in fileids:
            # hack
            # - we don't use .hgtags
            # - workingctx produces ids with length 42,
            #   which we skip since they aren't in any cache
            if (file == '.hgtags' or len(id) == 42
                or not repo.shallowmatch(file)):
                continue

            idstocheck.append((file, bin(id)))

        datastore = self.datastore
        historystore = self.historystore
        if force:
            datastore = unioncontentstore(*repo.shareddatastores)
            historystore = unionmetadatastore(*repo.sharedhistorystores)

        missingids = set()
        if fetchdata:
            missingids.update(datastore.getmissing(idstocheck))
        if fetchhistory:
            missingids.update(historystore.getmissing(idstocheck))

        # partition missing nodes into nullid and not-nullid so we can
        # warn about this filtering potentially shadowing bugs.
        nullids = len([None for unused, id in missingids if id == nullid])
        if nullids:
            missingids = [(f, id) for f, id in missingids if id != nullid]
            repo.ui.develwarn(
                ('remotefilelog not fetching %d null revs'
                 ' - this is likely hiding bugs' % nullids),
                config='remotefilelog-ext')
        if missingids:
            global fetches, fetched, fetchcost
            fetches += 1

            # We want to be able to detect excess individual file downloads, so
            # let's log that information for debugging.
            if fetches >= 15 and fetches < 18:
                if fetches == 15:
                    fetchwarning = self.ui.config('remotefilelog',
                                                  'fetchwarning')
                    if fetchwarning:
                        self.ui.warn(fetchwarning + '\n')
                self.logstacktrace()
            missingids = [(file, hex(id)) for file, id in missingids]
            fetched += len(missingids)
            start = time.time()
            missingids = self.request(missingids)
            if missingids:
                raise error.Abort(_("unable to download %d files") %
                                  len(missingids))
            fetchcost += time.time() - start

    def logstacktrace(self):
        import traceback
        self.ui.log('remotefilelog', 'excess remotefilelog fetching:\n%s',
                    ''.join(traceback.format_stack()))
