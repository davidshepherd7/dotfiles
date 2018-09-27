Skip test if remotenames not present
  $ . $TESTDIR/require-ext.sh remotenames

Set up extensions (order is important here, we must test tweakdefaults loading last)
  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > rebase=
  > remotenames=
  > tweakdefaults=$TESTDIR/../hgext3rd/tweakdefaults.py
  > EOF

Run test
  $ hg init repo
  $ cd repo
  $ touch a
  $ hg commit -Aqm a
  $ touch b
  $ hg commit -Aqm b
  $ hg bookmark AB
  $ hg up ".^"
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  (leaving bookmark AB)
  (hint: use 'hg prev' to move to the parent changeset)
  $ touch c
  $ hg commit -Aqm c
  $ hg bookmark C -t AB
  $ hg rebase
  rebasing 2:d5e255ef74f8 "c" (C tip)
  saved backup bundle to $TESTTMP/repo/.hg/strip-backup/d5e255ef74f8-7d2cc323-rebase.hg (glob)


