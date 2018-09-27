# arcdiff.py - extension adding an option to the diff command to show changes
#              since the last arcanist diff
#
# Copyright 2016 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

import os

from mercurial import commands, error, extensions
from mercurial.i18n import _

from phabricator import (
    arcconfig,
    graphql,
    diffprops,
)

def extsetup(ui):
    entry = extensions.wrapcommand(commands.table, 'diff', _diff)
    options = entry[1]
    options.append(('', 'since-last-arc-diff', None,
                    _('show changes since last `arc diff`')))

def _differentialhash(ui, repo, phabrev):
    timeout = repo.ui.configint('ssl', 'timeout', 5)
    ca_certs = repo.ui.configpath('web', 'cacerts')
    try:
        client = graphql.Client(
            repodir=repo.root, ca_bundle=ca_certs, repo=repo)
        info = client.getrevisioninfo(timeout, [phabrev]).get(str(phabrev))
        if not info:
            return None
        return info

    except graphql.ClientError as e:
        ui.warn(_('Error calling graphql: %s\n') % str(e))
        return None
    except arcconfig.ArcConfigError as e:
        raise error.Abort(str(e))

def _diff(orig, ui, repo, *pats, **opts):
    if not opts.get('since_last_arc_diff'):
        return orig(ui, repo, *pats, **opts)

    ctx = repo['.']
    phabrev = diffprops.parserevfromcommitmsg(ctx.description())

    if phabrev is None:
        mess = _('local changeset is not associated with a differential '
                 'revision')
        raise error.Abort(mess)

    rev = _differentialhash(ui, repo, phabrev)

    if rev is None or not isinstance(rev, dict) or "hash" not in rev:
        mess = _('unable to determine previous changeset hash')
        raise error.Abort(mess)

    rev = str(rev['hash'])
    opts['rev'] = [rev]

    # if patterns aren't provided, restrict diff to files in both changesets
    # this prevents performing a diff on rebased changes
    if len(pats) == 0:
        prev = set(repo.unfiltered()[rev].files())
        curr = set(repo['.'].files())
        pats = tuple(os.path.join(repo.root, p) for p in prev | curr)

    return orig(ui, repo, *pats, **opts)
