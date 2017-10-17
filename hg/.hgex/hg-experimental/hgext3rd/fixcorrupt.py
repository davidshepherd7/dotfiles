# fixcorrupt.py
#
# Copyright 2017 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from __future__ import absolute_import

import time

from mercurial.i18n import _
from mercurial import (
    encoding,
    error,
    registrar,
    revlog,
)

testedwith = 'ships-with-fb-hgext'

cmdtable = {}
command = registrar.command(cmdtable)

def quickchecklog(ui, log, name, knownbroken):
    """
    knownbroken: a set of known broken *changelog* revisions

    returns (rev, linkrev) of the first bad entry
    returns (None, None) if nothing is bad
    """
    lookback = 10
    rev = max(0, len(log) - lookback)
    numchecked = 0
    seengood = False
    topic = _('checking %s') % name
    while rev < len(log):
        numchecked += 1
        ui.progress(topic, numchecked, item=rev)
        linkrev = log.linkrev(rev)
        if linkrev in knownbroken:
            ui.write(_('%s: marked corrupted at rev %d (linkrev=%d)\n')
                     % (name, rev, linkrev))
            return rev, linkrev
        try:
            log.revision(rev, raw=True)
            seengood = True
            rev += 1
        except Exception: #  RevlogError, mpatchError, ValueError, etc
            if rev == 0:
                raise error.RevlogError(_('all %s entries appear corrupt!') %
                                        (name,))
            if not seengood:
                # If the earliest rev we looked at is bad, look back farther
                lookback *= 2
                rev = max(0, len(log) - lookback)
                continue
            ui.write(_('%s: corrupted at rev %d (linkrev=%d)\n')
                     % (name, rev, linkrev))
            return rev, linkrev
    ui.write(_('%s looks okay\n') % name)
    return None, None

def truncate(ui, repo, path, size, dryrun=True, backupprefix=''):
    oldsize = repo.svfs.stat(path).st_size
    if oldsize == size:
        return
    if oldsize < size:
        ui.write(_('%s: bad truncation request: %s to %s bytes\n')
                 % (path, oldsize, size))
        return
    ui.write(_('truncating %s from %s to %s bytes\n')
             % (path, oldsize, size))
    if dryrun:
        return

    repo.vfs.makedirs('truncate-backups')
    with repo.svfs.open(path, 'ab+') as f:
        f.seek(size)
        # backup the part being truncated
        backuppart = f.read(oldsize - size)
        if len(backuppart) != oldsize - size:
            raise error.Abort(_('truncate: cannot backup confidently'))
        with repo.vfs.open('truncate-backups/%s%s.backup-byte-%s-to-%s' %
                           (backupprefix, repo.svfs.basename(path), size,
                            oldsize), 'w') as bkf:
            bkf.write(backuppart)
        f.truncate(size)

@command('debugfixcorrupt',
         [('', 'no-dryrun', None, _('write changes (destructive)')),
         ], _('[OPTION]... [REV [FILE]...]'))
def fixcorrupt(ui, repo, *args, **opts):
    """
    Try to fix a corrupted repo by removing bad revisions at the end of
    changelog and manifest. Only works with remotefilelog repos.
    """
    # the extension only checks manifest and changelog, so it only works with
    # remotefilelog.
    if ('remotefilelog' not in repo.requirements
        and not encoding.environ.get('SKIPREMOTEFILELOGCHECK')):
        raise error.Abort(_('only remotefilelog repo is supported'))

    dryrun = not opts['no_dryrun']

    # we may access hidden nodes
    repo = repo.unfiltered()

    # only interested in these 2 revlogs
    logs = [('changelog', repo.changelog),
            ('manifest', repo.manifestlog._revlog)]

    # ensure they are REVLOGV1 and do not use inline index
    for name, log in logs:
        if (log.version & 0xFFFF) != revlog.REVLOGV1:
            raise error.Abort(_('%s: unsupported revlog version %d')
                              % (name, log.version & 0xffff))
        if log._inline:
            raise error.Abort(_('%s: inline index is not supported') % (name))
        if repo.svfs.stat(log.indexfile).st_size // 64 != len(log):
            raise error.Abort(_('unexpected index size for %s') % name)

    # check changelog first, then manifest. manifest revisions with a bad
    # linkrev is also marked broken, even if passes hash check.
    badrevs = {}
    knownbadrevs = set()
    for name, log in logs:
        rev, linkrev = quickchecklog(ui, log, name, knownbadrevs)
        if rev is None:
            continue
        # sanity check
        if rev >= len(log):
            raise error.Abort(_('%s index is corrupted') % name)
        # save the rev numbers
        badrevs[name] = (rev, linkrev)
        knownbadrevs.add(linkrev)

    if not badrevs:
        ui.write(_('nothing to do\n'))
        return 1

    # sync broken revisions from manifest to changelog
    if 'manifest' in badrevs:
        badlinkrev = badrevs['manifest'][1]
        badrevs['changelog'] = (badlinkrev, badlinkrev)

    # truncate revlogs
    backupprefix = '%s-' % int(time.time())
    with repo.wlock(), repo.lock():
        repo.destroying()
        for name, log in logs:
            rev, linkrev = badrevs[name]
            ui.write(_('%s: will lose %d revisions\n')
                     % (name, len(log) - 1 - rev))
            truncate(ui, repo, log.datafile, log.start(rev), dryrun,
                     backupprefix)
            truncate(ui, repo, log.indexfile, rev * 64, dryrun,
                     backupprefix)
        if dryrun:
            ui.write(_('re-run with --no-dryrun to fix.\n'))
        else:
            ui.write(_('fix completed. re-run to check more revisions.\n'))
        repo.destroyed()
