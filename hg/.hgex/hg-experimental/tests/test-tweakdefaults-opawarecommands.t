  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > tweakdefaults=$TESTDIR/../hgext3rd/tweakdefaults.py
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > rebase=
  > [experimental]
  > evolution=createmarkers
  > EOF

Setup repo
  $ hg init opawarerepo
  $ cd opawarerepo
  $ echo root > root && hg ci -Am root
  adding root

Check amend metadata
  $ echo a > a && hg ci -Am a
  adding a
  $ echo aa > a && hg amend
  $ hg debugobsolete
  .* {'ef1': '40', 'operation': 'amend', 'user': 'test'} (re)

Check rebase metadata
  $ hg book -r . destination
  $ hg up 0
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ echo b > b && hg ci -Am b
  adding b
  created new head
  $ hg rebase -r . -d destination
  rebasing 3:1e9a3c00cbe9 "b" (tip)
  $ hg debugobsolete
  .* {'ef1': '40', 'operation': 'amend', 'user': 'test'} (re)
  .* {'ef1': '36', 'operation': 'rebase', 'user': 'test'} (re)
