# movement.py - commands to move working parent like previous, next, etc.
#
# Copyright 2016 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from __future__ import absolute_import

from itertools import count

from mercurial import (
    bookmarks,
    cmdutil,
    commands,
    error,
    phases,
    registrar,
    scmutil,
)
from mercurial.i18n import _
from mercurial.node import nullrev, short

from . import common

cmdtable = {}
command = registrar.command(cmdtable)

moveopts = [
    ('B', 'move-bookmark', False, _('move active bookmark')),
    ('', 'merge', False, _('merge uncommitted changes')),
]

@command(
    '^previous',
    [('', 'clean', False,
      _('discard uncommitted changes (no backup)')),
     ('', 'newest', False,
      _('always pick the newest parent when a changeset has multiple parents')),
     ('', 'bottom', False,
      _('update to the lowest non-public ancestor of the current changeset')),
     ('', 'bookmark', False,
      _('update to the first ancestor with a bookmark')),
     ('', 'no-activate-bookmark', False,
      _('do not activate the bookmark on the destination changeset')),
    ] + moveopts, _('[OPTIONS]... [STEPS]'))
def previous(ui, repo, *args, **opts):
    """update to parent changeset"""
    _moverelative(ui, repo, args, opts, reverse=True)

@command(
    '^next',
    [('', 'clean', False,
      _('discard uncommitted changes (no backup)')),
     ('', 'newest', False,
      _('always pick the newest child when a changeset has multiple children')),
     ('', 'rebase', False, _('rebase each changeset if necessary')),
     ('', 'top', False, _('update to the head of the current stack')),
     ('', 'bookmark', False,
      _('update to the first changeset with a bookmark')),
     ('', 'no-activate-bookmark', False,
      _('do not activate the bookmark on the destination changeset')),
     ('', 'towards', "", _('move linearly towards the specified head')),
    ] + moveopts, _('[OPTIONS]... [STEPS]'))
def next(ui, repo, *args, **opts):
    """update to child changeset"""
    _moverelative(ui, repo, args, opts, reverse=False)

def _moverelative(ui, repo, args, opts, reverse=False):
    """Update to a changeset relative to the current changeset.
       Implements both `hg previous` and `hg next`.

       Takes in a list of positional arguments and a dict of command line
       options. (See help for `hg previous` and `hg next` to see which
       arguments and flags are supported.)

       Moves forward through history by default -- the behavior of `hg next`.
       Setting reverse=True will change the behavior to that of `hg previous`.
    """
    # Parse positional argument.
    try:
        n = int(args[0]) if args else 1
    except ValueError:
        raise error.Abort(_("argument must be an integer"))
    if n <= 0:
        return

    if ui.configbool('fbamend', 'alwaysnewest'):
        opts['newest'] = True

    # Check that the given combination of arguments is valid.
    if args:
        if opts.get('bookmark', False):
            raise error.Abort(_("cannot use both number and --bookmark"))
        if opts.get('top', False):
            raise error.Abort(_("cannot use both number and --top"))
        if opts.get('bottom', False):
            raise error.Abort(_("cannot use both number and --bottom"))
    if opts.get('bookmark', False):
        if opts.get('top', False):
            raise error.Abort(_("cannot use both --top and --bookmark"))
        if opts.get('bottom', False):
            raise error.Abort(_("cannot use both --bottom and --bookmark"))
    if opts.get('towards', False) and opts.get('top', False):
        raise error.Abort(_("cannot use both --top and --towards"))
    if opts.get('merge', False) and opts.get('rebase', False):
        raise error.Abort(_("cannot use both --merge and --rebase"))

    # Check if there is an outstanding operation or uncommited changes.
    cmdutil.checkunfinished(repo)
    if not opts.get('clean', False) and not opts.get('merge', False):
        try:
            cmdutil.bailifchanged(repo)
        except error.Abort as e:
            e.hint = _("use --clean to discard uncommitted changes "
                       "or --merge to bring them along")
            raise

    # If we have both --clean and --rebase, we need to discard any outstanding
    # changes now before we attempt to perform any rebases.
    if opts.get('clean') and opts.get('rebase'):
        commands.update(ui, repo, rev=repo['.'].rev(), clean=True)

    with repo.wlock(), repo.lock():
        # Record the active bookmark, if any.
        bookmark = repo._activebookmark
        noactivate = opts.get('no_activate_bookmark', False)
        movebookmark = opts.get('move_bookmark', False)

        with repo.transaction('moverelative') as tr:
            # Find the desired changeset. May potentially perform rebase.
            try:
                target = _findtarget(ui, repo, n, opts, reverse)
            except error.InterventionRequired:
                # Rebase failed. Need to manually close transaction to allow
                # `hg rebase --continue` to work correctly.
                tr.close()
                raise

            # Move the active bookmark if neccesary. Needs to happen before
            # we update to avoid getting a 'leaving bookmark X' message.
            if movebookmark and bookmark is not None:
                _setbookmark(repo, tr, bookmark, target)

            # Update to the target changeset.
            commands.update(ui, repo, rev=target,
                            clean=opts.get('clean', False))

            # Print out the changeset we landed on.
            _showchangesets(ui, repo, revs=[target])

            # Activate the bookmark on the new changeset.
            if not noactivate and not movebookmark:
                _activate(ui, repo, target)

def _findtarget(ui, repo, n, opts, reverse):
    """Find the appropriate target changeset for `hg previous` and
       `hg next` based on the provided options. May rebase the traversed
       changesets if the rebase option is given in the opts dict.
    """
    towards = opts.get('towards')
    newest = opts.get('newest', False)
    bookmark = opts.get('bookmark', False)
    rebase = opts.get('rebase', False)
    top = opts.get('top', False)
    bottom = opts.get('bottom', False)

    if top and not rebase:
        # If we're not rebasing, jump directly to the top instead of
        # walking up the stack.
        return _findstacktop(ui, repo, newest)
    elif bottom:
        return _findstackbottom(ui, repo)
    elif reverse:
        return _findprevtarget(ui, repo, n, bookmark, newest)
    else:
        return _findnexttarget(ui, repo, n, bookmark, newest, rebase, top,
                               towards)

def _findprevtarget(ui, repo, n=None, bookmark=False, newest=False):
    """Get the revision n levels down the stack from the current revision.
       If newest is True, if a changeset has multiple parents the newest
       will always be chosen. Otherwise, throws an exception.
    """
    ctx = repo['.']

    # The caller must specify a stopping condition -- either a number
    # of steps to walk or a bookmark to search for.
    if not n and not bookmark:
        raise error.Abort(_("no stop condition specified"))

    for i in count(0):
        # Loop until we're gone the desired number of steps, or we reach a
        # node with a bookmark if the bookmark option was specified.
        if bookmark:
            if i > 0 and ctx.bookmarks():
                break
        elif i >= n:
            break

        parents = ctx.parents()

        # Is this the root of the current branch?
        if not parents or parents[0].rev() == nullrev:
            if ctx.rev() == repo['.'].rev():
                raise error.Abort(_("current changeset has no parents"))
            ui.status(_('reached root changeset\n'))
            break

        # Are there multiple parents?
        if len(parents) > 1 and not newest:
            ui.status(_("changeset %s has multiple parents, namely:\n")
                      % short(ctx.node()))
            _showchangesets(ui, repo, contexts=parents)
            raise error.Abort(_("ambiguous previous changeset"),
                              hint=_("use the --newest flag to always "
                                     "pick the newest parent at each step"))

        # Get the parent with the highest revision number.
        ctx = max(parents, key=lambda x: x.rev())

    return ctx.rev()

def _findnexttarget(ui, repo, n=None, bookmark=False, newest=False,
                    rebase=False, top=False, towards=None):
    """Get the revision n levels up the stack from the current revision.
       If newest is True, if a changeset has multiple children the newest
       will always be chosen. Otherwise, throws an exception. If the rebase
       option is specified, potentially rebase unstable children as we
       walk up the stack.
    """
    rev = repo['.'].rev()

    # The caller must specify a stopping condition -- either a number
    # of steps to walk, a bookmark to search for, or --top.
    if not n and not bookmark and not top:
        raise error.Abort(_("no stop condition specified"))

    # Precompute child relationships to avoid expensive ctx.children() calls.
    if not rebase:
        childrenof = common.getchildrelationships(repo, [rev])

    # If we're moving towards a rev, get the chain of revs up to that rev.
    line = set()
    if towards:
        towardsrevs = scmutil.revrange(repo, [towards])
        if len(towardsrevs) > 1:
            raise error.Abort(_("'%s' refers to multiple changesets")
                              % towards)
        towardsrev = towardsrevs.first()
        line = set(repo.revs('.::%d', towardsrev))
        if not line:
            raise error.Abort(
                _("the current changeset is not an ancestor of '%s'")
                % towards)

    for i in count(0):
        # Loop until we're gone the desired number of steps, or we reach a
        # node with a bookmark if the bookmark option was specified.
        # If top is specified, loop until we reach a head.
        if bookmark:
            if i > 0 and repo[rev].bookmarks():
                break
        elif i >= n and not top:
            break

        # If the rebase flag is present, rebase any unstable children.
        # This means we can't rely on precomputed child relationships.
        if rebase:
            common.restackonce(ui, repo, rev, childrenonly=True)
            children = set(c.rev() for c in repo[rev].children())
        else:
            children = childrenof[rev]

        # Remove children not along the specified line.
        children = (children & line) or children

        # Have we reached a head?
        if not children:
            if rev == repo['.'].rev():
                raise error.Abort(_("current changeset has no children"))
            if not top:
                ui.status(_('reached head changeset\n'))
            break

        # Are there multiple children?
        if len(children) > 1 and not newest:
            ui.status(_("changeset %s has multiple children, namely:\n")
                      % short(repo[rev].node()))
            _showchangesets(ui, repo, revs=children)
            raise error.Abort(_("ambiguous next changeset"),
                              hint=_("use the --newest or --towards flags "
                                     "to specify which child to pick"))

        # Get the child with the highest revision number.
        rev = max(children)

    return rev

def _findstacktop(ui, repo, newest=False):
    """Find the head of the current stack."""
    heads = repo.revs('heads(.::)')
    if len(heads) > 1:
        if newest:
            # We can't simply return heads.max() since this might give
            # a different answer from walking up the stack as in
            # _findnexttarget(), which picks the child with the greatest
            # revision number at each step. This would be confusing, since
            # it would mean that `hg next --top` and `hg next --top --rebase`
            # would result in different destination changesets.
            return _findnexttarget(ui, repo, newest=True, top=True)
        ui.warn(_("current stack has multiple heads, namely:\n"))
        _showchangesets(ui, repo, revs=heads)
        raise error.Abort(_("ambiguous next changeset"),
                          hint=_("use the --newest flag to always "
                                 "pick the newest child at each step"))
    return heads.first()

def _findstackbottom(ui, repo):
    """Find the lowest non-public ancestor of the current changeset."""
    if repo['.'].phase() == phases.public:
        raise error.Abort(_("current changeset is public"))
    return repo.revs("::. & draft()").first()

def _showchangesets(ui, repo, contexts=None, revs=None):
    """Pretty print a list of changesets. Can take either a list of
       change contexts or a list of revision numbers.
    """
    if contexts is None:
        contexts = []
    if revs is not None:
        contexts.extend(repo[r] for r in revs)
    showopts = {
        'template': '[{shortest(node, 6)}] {if(bookmarks, "({bookmarks}) ")}'
                    '{desc|firstline}\n'
    }
    displayer = cmdutil.show_changeset(ui, repo, showopts)
    for ctx in contexts:
        displayer.show(ctx)

def _setbookmark(repo, tr, bookmark, rev):
    """Make the given bookmark point to the given revision."""
    node = repo.changelog.node(rev)
    repo._bookmarks[bookmark] = node
    repo._bookmarks.recordchange(tr)

def _activate(ui, repo, rev):
    """Activate the bookmark on the given revision if it only has one bookmark.
    """
    ctx = repo[rev]
    marks = repo.nodebookmarks(ctx.node())
    if len(marks) == 1:
        b = ui.label(marks[0], 'bookmarks.active')
        ui.status(_("(activating bookmark %s)\n") % b)
        bookmarks.activate(repo, marks[0])
