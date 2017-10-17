# unamend.py - undo an amend operation
#
# Copyright 2016 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from __future__ import absolute_import

from mercurial import (
    error,
    extensions,
    obsolete,
    obsutil,
    registrar,
)
from mercurial.i18n import _

cmdtable = {}
command = registrar.command(cmdtable)

def precursormarkers(ctx):
    """obsolete marker marking this changeset as a successors"""
    for data in ctx.repo().obsstore.precursors.get(ctx.node(), ()):
        yield obsutil.marker(ctx.repo(), data)

@command('^unamend', [])
def unamend(ui, repo, **opts):
    """undo the amend operation on a current changeset

    This command will roll back to the previous version of a changeset,
    leaving working directory in state in which it was before running
    `hg amend` (e.g. files modified as part of an amend will be
    marked as modified `hg status`)"""
    try:
        extensions.find('inhibit')
    except KeyError:
        hint = _("please add inhibit to the list of enabled extensions")
        e = _("unamend requires inhibit extension to be enabled")
        raise error.Abort(e, hint=hint)

    unfi = repo.unfiltered()

    # identify the commit from which to unamend
    curctx = repo['.']

    # identify the commit to which to unamend
    markers = list(precursormarkers(curctx))
    if len(markers) != 1:
        e = _("changeset must have one precursor, found %i precursors")
        raise error.Abort(e % len(markers))

    precnode = markers[0].precnode()
    precctx = unfi[precnode]

    if curctx.children():
        raise error.Abort(_("cannot unamend in the middle of a stack"))

    with repo.wlock(), repo.lock():
        ctxbookmarks = curctx.bookmarks()
        changedfiles = []
        wctx = repo[None]
        wm = wctx.manifest()
        cm = precctx.manifest()
        dirstate = repo.dirstate
        diff = cm.diff(wm)
        changedfiles.extend(diff.iterkeys())

        tr = repo.transaction('unamend')
        with dirstate.parentchange():
            dirstate.rebuild(precnode, cm, changedfiles)
            # we want added and removed files to be shown
            # properly, not with ? and ! prefixes
            for filename, data in diff.iteritems():
                if data[0][0] is None:
                    dirstate.add(filename)
                if data[1][0] is None:
                    dirstate.remove(filename)
        changes = []
        for book in ctxbookmarks:
            changes.append((book, precnode))
        repo._bookmarks.applychanges(repo, tr, changes)
        obsolete.createmarkers(repo, [(curctx, (precctx,))])
        tr.close()
