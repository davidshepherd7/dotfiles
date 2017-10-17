  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > tweakdefaults=$TESTDIR/../hgext3rd/tweakdefaults.py
  > rebase=
  > [experimental]
  > updatecheck=noconflict
  > EOF

Set up the repository.
  $ hg init repo
  $ cd repo
  $ hg debugbuilddag -m '+4 *3 +1'
  $ hg log --graph -r 0:: -T '{rev}'
  o  5
  |
  o  4
  |
  | o  3
  | |
  | o  2
  |/
  o  1
  |
  o  0
  
  $ hg up 3
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

Make an uncommitted change.
  $ echo foo > foo
  $ hg add foo
  $ hg st
  A foo

Can always update to current commit.
  $ hg up .
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved

Abort with --check set, succeed with --merge
  $ hg up 2 --check
  abort: uncommitted changes
  [255]
  $ hg up --merge 2
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

Updates to other branches should fail without --merge.
  $ hg up 4 --check
  abort: uncommitted changes
  [255]
  $ hg up --merge 4
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

Certain flags shouldn't work together.
  $ hg up --check --merge 3
  abort: can only specify one of -C/--clean, -c/--check, or -m/merge
  [255]
  $ hg up --check --clean 3
  abort: can only specify one of -C/--clean, -c/--check, or -m/merge
  [255]
  $ hg up --clean --merge 3
  abort: can only specify one of -C/--clean, -c/--check, or -m/merge
  [255]

--clean should work as expected.
  $ hg st
  A foo
  $ hg up --clean 3
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg st
  ? foo
