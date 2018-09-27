  $ . "$TESTDIR/copytrace.sh"
  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > copytrace=$TESTDIR/../hgext3rd/copytrace.py
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > rebase=
  > shelve=
  > [fbamend]
  > userestack=True
  > [experimental]
  > copytrace=off
  > evolution=createmarkers
  > EOF

Test amend copytrace
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ hg add a
  $ hg ci -m "create a"
  $ echo b > a
  $ hg ci -qm "mod a"
  $ hg up -q ".^"
  $ hg mv a b
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg rebase --restack
  rebasing 2:ad25e018afa9 "mod a"
  merging b and a to b
  $ ls
  b
  x
  $ cat b
  a
  $ hg update 4
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat b
  b
  $ cd ..
  $ rm -rf repo

Test amend copytrace with multiple stacked commits
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ echo b > b
  $ echo c > c
  $ hg add a b c
  $ hg ci -m "create a b c"
  $ echo a1 > a
  $ hg ci -qm "mod a"
  $ echo b2 > b
  $ hg ci -qm "mod b"
  $ echo c3 > c
  $ hg ci -qm "mod c"
  $ hg bookmark test-top
  $ hg up -q '.~3'
  $ hg mv a a1
  $ hg mv b b2
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg mv c c3
  $ hg amend
  $ hg rebase --restack
  rebasing 2:797127d4e250 "mod a"
  merging a1 and a to a1
  rebasing 3:e2aabbfe749a "mod b"
  merging b2 and b to b2
  rebasing 4:4f8d18558559 "mod c" (test-top)
  merging c3 and c to c3
  $ hg up test-top
  3 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (activating bookmark test-top)
  $ cat a1 b2 c3
  a1
  b2
  c3
  $ cd ..
  $ rm -rf repo

Test amend copytrace with multiple renames of the same file
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ hg add a
  $ hg ci -m "create a"
  $ echo b > a
  $ hg ci -qm "mod a"
  $ hg up -q ".^"
  $ hg mv a b
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg mv b c
  $ hg amend
  $ hg rebase --restack
  rebasing 2:ad25e018afa9 "mod a"
  merging c and a to c
  $ hg update 5
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat c
  b
  $ cd ..
  $ rm -rf repo

Test amend copytrace with copies
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ echo i > i
  $ hg add a i
  $ hg ci -m "create a i"
  $ echo b > a
  $ hg ci -qm "mod a"
  $ echo j > i
  $ hg ci -qm "mod i"
  $ hg bookmark test-top
  $ hg up -q ".~2"
  $ hg cp a b
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg cp i j
  $ hg amend
  $ hg cp b c
  $ hg amend
  $ hg rebase --restack
  rebasing 2:6938f0d82b23 "mod a"
  merging b and a to b
  merging c and a to c
  rebasing 3:df8dfcb1d237 "mod i" (test-top)
  merging j and i to j
  $ hg up test-top
  5 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (activating bookmark test-top)
  $ cat a b c i j
  b
  b
  b
  j
  j
  $ cd ..
  $ rm -rf repo

Test rebase after amend deletion of copy
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ hg add a
  $ hg ci -m "create a"
  $ echo b > a
  $ hg ci -qm "mod a"
  $ hg up -q ".^"
  $ hg cp a b
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg rm b
  $ hg amend
  $ hg rebase --restack
  rebasing 2:ad25e018afa9 "mod a"
  $ cd ..
  $ rm -rf repo

Test failure to rebase deletion after rename
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ hg add a
  $ hg ci -m "create a"
  $ echo b > a
  $ hg ci -qm "mod a"
  $ hg rm a
  $ hg ci -m "delete a"
  $ hg up -q ".~2"
  $ hg mv a b
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg rebase --restack
  rebasing 2:ad25e018afa9 "mod a"
  merging b and a to b
  rebasing 3:ba0395f0e180 "delete a"
  abort: a@ba0395f0e180: not found in manifest!
  [255]
  $ hg rebase --abort
  saved backup bundle to $TESTTMP/repo/.hg/strip-backup/3fd0353a7967-a25c7d46-backup.hg (glob)
  rebase aborted
  $ cd ..
  $ rm -rf repo

Test amend copytrace can be disabled
  $ cat >> $HGRCPATH << EOF
  > [copytrace]
  > enableamendcopytrace=false
  > EOF
  $ hg init repo
  $ initclient repo
  $ cd repo
  $ echo x > x
  $ hg add x
  $ hg ci -m initial
  $ echo a > a
  $ hg add a
  $ hg ci -m "create a"
  $ echo b > a
  $ hg ci -qm "mod a"
  $ hg up -q ".^"
  $ hg mv a b
  $ hg amend
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg rebase --restack
  rebasing 2:ad25e018afa9 "mod a"
  other [source] changed a which local [dest] deleted
  hint: if this message is due to a moved file, you can ask mercurial to attempt to automatically resolve this change by re-running with the --tracecopies flag, but this will significantly slow down the operation, so you will need to be patient.
  Source control team is working on fixing this problem.
  use (c)hanged version, leave (d)eleted, or leave (u)nresolved? u
  unresolved conflicts (see hg resolve, then hg rebase --continue)
  [1]
  $ cd ..
  $ rm -rf repo
