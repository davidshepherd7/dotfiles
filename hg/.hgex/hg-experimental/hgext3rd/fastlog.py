# fastlog.py
#
# An extension to query remote servers for logs using scmquery / fastlog
#
# Copyright 2016 Facebook, Inc.
"""
connect to scmquery servers for fast fetching of logs on files and directories.

Configure it by adding the following config options to your .hg/hgrc.
This relies on fbconduit being setup for the repo; this should already
be configured if supported by your repo.

[fastlog]
enabled=true
"""

from mercurial import (
    changelog,
    cmdutil,
    error,
    extensions,
    node,
    phases,
    revset,
    scmutil,
    smartset,
    util
)
from mercurial.i18n import _
from mercurial.node import nullrev

import heapq
from threading import Thread, Event
from collections import deque

conduit = None

FASTLOG_MAX = 500
FASTLOG_QUEUE_SIZE = 1000
FASTLOG_TIMEOUT = 20

def extsetup(ui):
    global conduit
    try:
        conduit = extensions.find("fbconduit")
    except KeyError:
        from hgext3rd import fbconduit as conduit
    except ImportError:
        ui.warn(_('Unable to find fbconduit extension\n'))
        return
    if not util.safehasattr(conduit, 'conduit_config'):
        ui.warn(_('Incompatible conduit module; disabling fastlog\n'))
        return
    if not conduit.conduit_config(ui):
        ui.warn(_('No conduit host specified in config; disabling fastlog\n'))
        return

    extensions.wrapfunction(cmdutil, 'getlogrevs', getfastlogrevs)

def lazyparents(rev, public, parentfunc):
    """lazyparents(rev, public)
    Lazily yield parents of rev in reverse order until all nodes
    in public have been reached or all revs have been exhausted

    10
     | \
     9  8
     |  | \
     7  6  5
     |  | /
     4 *3   First move, 4 -3
     | /
     2 *2   Second move, 4 -1
     | *
     1

    For example:
    >>> parents = { 10:[9, 8], 9:[7], 8:[6,5], 7:[4], 6:[3], 5:[3], 4:[2] }
    >>> parents.update({ 3:[2], 2:[1], 1:[] })
    >>> parentfunc = lambda k: parents[k]
    >>> public = set([1])
    >>> for p in lazyparents(10, public, parentfunc): print p,
    10 9 8 7 6 5 4 3 2 1
    >>> public = set([2,3])
    >>> for p in lazyparents(10, public, parentfunc): print p,
    10 9 8 7 6 5 4 3 2
    >>> parents[4] = [3]
    >>> public = set([3,4,5])
    >>> for p in lazyparents(10, public, parentfunc): print p,
    10 9 8 7 6 5 4 3
    >>> parents[4] = [1]
    >>> public = set([3,5,7])
    >>> for p in lazyparents(10, public, parentfunc): print p,
    10 9 8 7 6 5 4 3 2 1
    """
    seen = set()
    heap = [-rev]

    while heap:
        cur = -heapq.heappop(heap)
        if cur not in seen:
            seen.add(cur)
            yield cur

            published = cur in public
            if published:
                # Down to one public ancestor; end generation
                if len(public) == 1:
                    return
                public.remove(cur)

            for p in parentfunc(cur):
                if p != nullrev:
                    heapq.heappush(heap, -p)
                    if published:
                        public.add(p)

def dirmatches(files, paths):
    """dirmatches(files, paths)
    Return true if any files match directories in paths
    Expects paths to be appended by '/'

    >>> dirmatches(['holy/grail'], ['holy/'])
    True
    >>> dirmatches(['holy/grail'], ['holly/'])
    False
    >>> try: dirmatches(['holy/grail'], ['holy'])
    ... except AssertionError, e: print('caught')
    caught
    """
    assert paths
    for path in paths:
        assert path[-1] == '/'
        for f in files:
            if f.startswith(path):
                return True
    return False

def originator(parentfunc, rev):
    """originator(repo, rev)
    Yield parents of rev from repo in reverse order
    """
    # Use set(nullrev, rev) to iterate until termination
    for p in lazyparents(rev, set([nullrev, rev]), parentfunc):
        if rev != p:
            yield p

def getfastlogrevs(orig, repo, pats, opts):
    blacklist = ['all', 'branch', 'rev', 'sparse']
    if any(opts.get(opt) for opt in blacklist) or not opts.get('follow'):
        return orig(repo, pats, opts)

    reponame = repo.ui.config('fbconduit', 'reponame')
    if reponame and repo.ui.configbool('fastlog', 'enabled'):
        wctx = repo[None]
        match, pats = scmutil.matchandpats(wctx, pats, opts)
        files = match.files()
        if not files or '.' in files:
            # Walking the whole repo - bail on fastlog
            return orig(repo, pats, opts)

        dirs = set()
        wvfs = repo.wvfs
        for path in files:
            if wvfs.isdir(path) and not wvfs.islink(path):
                dirs.update([path + '/'])
            else:
                # bail on symlinks, and also bail on files for now
                # with follow behavior, for files, we are supposed
                # to track copies / renames, but it isn't convenient
                # to do this through scmquery
                return orig(repo, pats, opts)

        rev = repo['.'].rev()

        parents = repo.changelog.parentrevs
        public = set()

        # Our criterion for invoking fastlog is finding a single
        # common public ancestor from the current head.  First we
        # have to walk back through drafts to find all interesting
        # public parents.  Typically this will just be one, but if
        # there are merged drafts, we may have multiple parents.
        if repo[rev].phase() == phases.public:
            public.add(rev)
        else:
            queue = deque()
            queue.append(rev)
            seen = set()
            while queue:
                cur = queue.popleft()
                if cur not in seen:
                    seen.add(cur)
                    if repo[cur].mutable():
                        for p in parents(cur):
                            if p != nullrev:
                                queue.append(p)
                    else:
                        public.add(cur)

        def fastlog(repo, startrev, dirs, localmatch):
            filefunc = repo.changelog.readfiles
            for parent in lazyparents(startrev, public, parents):
                files = filefunc(parent)
                if dirmatches(files, dirs):
                    yield parent
            repo.ui.debug('found common parent at %s\n' % repo[parent].hex())
            for rev in combinator(repo, parent, dirs, localmatch):
                yield rev

        def combinator(repo, rev, dirs, localmatch):
            """combinator(repo, rev, dirs, localmatch)
            Make parallel local and remote queries along ancestors of
            rev along path and combine results, eliminating duplicates,
            restricting results to those which match dirs
            """
            LOCAL = 'L'
            REMOTE = 'R'
            queue = util.queue(FASTLOG_QUEUE_SIZE + 100)
            hash = repo[rev].hex()

            local = LocalIteratorThread(queue, LOCAL, rev,
                                        dirs, localmatch, repo)
            remote = FastLogThread(queue, REMOTE, reponame, 'hg', hash, dirs,
                                   repo)

            # Allow debugging either remote or local path
            debug = repo.ui.config('fastlog', 'debug')
            if debug != 'local':
                repo.ui.debug('starting fastlog at %s\n' % hash)
                remote.start()
            if debug != 'remote':
                local.start()
            seen = set([rev])

            try:
                while True:
                    try:
                        producer, success, msg = queue.get(True, 3600)
                    except util.empty:
                        raise error.Abort("Timeout reading log data")
                    if not success:
                        if producer == LOCAL:
                            raise error.Abort(msg)
                        elif msg:
                            repo.ui.log("hgfastlog", msg)
                            continue

                    if msg is None:
                        # Empty message means no more results
                        return

                    rev = msg
                    if debug:
                        if producer == LOCAL:
                            repo.ui.debug('LOCAL:: %s\n' % msg)
                        elif producer == REMOTE:
                            repo.ui.debug('REMOTE:: %s\n' % msg)

                    if rev not in seen:
                        seen.add(rev)
                        yield rev
            finally:
                local.stop()
                remote.stop()

        # Complex match - use a revset.
        complex = ['date', 'exclude', 'include', 'keyword', 'no_merges',
                   'only_merges', 'prune', 'user']
        if match.anypats() or any(opts.get(opt) for opt in complex):
            f = fastlog(repo, rev, dirs, None)
            revs = smartset.generatorset(f, iterasc=False)
            revs.reverse()
            if not revs:
                return smartset.baseset([]), None, None
            expr, filematcher = cmdutil._makelogrevset(repo, pats, opts, revs)
            matcher = revset.match(repo.ui, expr)
            matched = matcher(repo, revs)
            return matched, expr, filematcher
        else:
            # Simple match without revset shaves ~0.5 seconds off
            # hg log -l 100 -T ' ' on common directories.
            expr = 'fastlog(%s)' % ','.join(dirs)
            return fastlog(repo, rev, dirs, dirmatches), expr, None

    return orig(repo, pats, opts)


class readonlychangelog(object):
    def __init__(self, opener):
        self._changelog = changelog.changelog(opener)

    def parentrevs(self, rev):
        return self._changelog.parentrevs(rev)

    def readfiles(self, node):
        return self._changelog.readfiles(node)

    def rev(self, node):
        return self._changelog.rev(node)


class LocalIteratorThread(Thread):
    """Class which reads from an iterator and sends results to a queue.

    Results are sent in a tuple (tag, success, result), where tag is the
    id passed to this class' initializer, success is a bool, True for
    success, False on error, and result is the output of the iterator.

    When the iterator is finished, a poison pill is sent to the queue
    with result set to None to signal completion.

    Used to allow parallel fetching of results from both a local and
    remote source.

    * queue - self explanatory
    * id - tag to use when sending messages
    * rev - rev to start iterating at
    * dirs - directories against which to match
    * localmatch - a function to match candidate results
    * repo - mercurial repository object

    If an exception is thrown, error result with the message from the
    exception will be passed along the queue.  Since local results are
    not expected to generate exceptions, this terminates iteration.
    """

    def __init__(self, queue, id, rev, dirs, localmatch, repo):
        Thread.__init__(self)
        self.daemon = True
        self.queue = queue
        self.id = id
        self.rev = rev
        self.dirs = dirs
        self.localmatch = localmatch
        self.ui = repo.ui
        self._stop = Event()

        # Create a private instance of changelog to avoid trampling
        # internal caches of other threads
        c = readonlychangelog(repo.svfs)
        self.generator = originator(c.parentrevs, rev)
        self.filefunc = c.readfiles
        self.ui = repo.ui

    def stop(self):
        self._stop.set()

    def stopped(self):
        return self._stop.isSet()

    def run(self):
        generator = self.generator
        match = self.localmatch
        dirs = self.dirs
        filefunc = self.filefunc
        queue = self.queue

        try:
            for result in generator:
                if self.stopped():
                    break
                if not match or match(filefunc(result), dirs):
                    queue.put((self.id, True, result))
        except Exception as e:
            self.ui.traceback()
            queue.put((self.id, False, str(e)))
        finally:
            queue.put((self.id, True, None))


class FastLogThread(Thread):
    """Class which talks to a remote SCMQuery

    Like the above, results are sent to a queue, and tagged with the
    id passed to this class' initializer.  Same rules for termination.

    We page results in windows of up to FASTLOG_MAX to avoid generating
    too many results; this has been optimized on the server to cache
    fast continuations but this assumes service stickiness.

    * queue - self explanatory
    * id - tag to use when sending messages
    * reponame - repository name (str)
    * scm - scm type (str)
    * rev - revision to start logging from
    * paths - paths to request logs
    * repo - mercurial repository object
    """

    def __init__(self, queue, id, reponame, scm, rev, paths, repo):
        Thread.__init__(self)
        self.daemon = True
        self.queue = queue
        self.id = id
        self.reponame = reponame
        self.scm = scm
        self.rev = rev
        self.paths = list(paths)
        self.ui = repo.ui
        self.changelog = readonlychangelog(repo.svfs)
        self._stop = Event()
        self._paths_to_fetch = 0

    def stop(self):
        self._stop.set()

    def stopped(self):
        return self._stop.isSet()

    def finishpath(self, path):
        self._paths_to_fetch -= 1

    def gettodo(self):
        return max(FASTLOG_MAX / self._paths_to_fetch, 100)

    def generate(self, path):
        start = str(self.rev)
        reponame = self.reponame
        revfn = self.changelog.rev
        skip = 0

        while True:
            if self.stopped():
                break

            results = None
            todo = self.gettodo()
            try:
                results = conduit.call_conduit(
                    'scmquery.log_v2',
                    repo=reponame,
                    scm_type=self.scm,
                    rev=start,
                    file_paths=[path],
                    skip=skip,
                    number=todo,
                )
            except Exception as e:
                if self.ui.config('fastlog', 'debug'):
                    self.ui.traceback(force=True)
                self.queue.put((self.id, False, str(e)))
                self.stop()
                return

            if results is None:
                self.queue.put((self.id, False, 'Unknown error'))
                self.stop()
                return

            for result in results:
                hash = result['hash']
                try:
                    if len(hash) != 40:
                        raise ValueError('Received invalid hash %s' % hash)
                    rev = revfn(node.bin(hash))
                    if rev is None:
                        raise KeyError('Hash %s not in local repo' % hash)
                except Exception as e:
                    if self.ui.config('fastlog', 'debug'):
                        self.ui.traceback(force=True)
                    self.queue.put((self.id, False, str(e)))
                else:
                    yield rev

            skip += todo
            if len(results) < todo:
                self.finishpath(path)
                return

    def run(self):
        revs = None
        paths = self.paths

        self._paths_to_fetch = len(paths)
        for path in paths:
            g = self.generate(path)
            gen = smartset.generatorset(g, iterasc=False)
            gen.reverse()
            if revs:
                revs = smartset.addset(revs, gen, ascending=False)
            else:
                revs = gen

        for rev in revs:
            if self.stopped():
                break
            self.queue.put((self.id, True, rev))
        # The end marker (self.id, True, None) indicates that the thread
        # completed successfully. Don't send it if the thread is stopped.
        # The thread can be stopped for one of two reasons:
        #  1. The fastlog service failed - in this case, flagging a successful
        #     finish is harmful, because it will stop us continuing with local
        #     results, truncating output.
        #  2. The caller is going to ignore all future results from us. In this
        #     case, it'll ignore the end marker anyway - it's discarding the
        #     entire queue.
        if not self.stopped():
            self.queue.put((self.id, True, None))

if __name__ == '__main__':
    import doctest
    doctest.testmod()
