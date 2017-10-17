  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > mergedriver=$TESTDIR/../hgext3rd/mergedriver.py
  > EOF

basic merge driver: just lists out files and contents, doesn't resolve any files

  $ cat > mergedriver-list.py << EOF
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >    for f in mergestate:
  >        repo.ui.status('%s %s\n' % (mergestate[f].upper(), f))
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     pass
  > EOF

  $ hg init repo1
  $ cd repo1
  $ echo afoo > foo.txt
  $ echo abar > bar.txt
  $ hg add foo.txt bar.txt
  $ hg commit -ma
  $ echo bfoo >> foo.txt
  $ echo bbar >> bar.txt
  $ hg commit -mb
  $ hg up 0
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ echo cfoo >> foo.txt
  $ echo cbar >> bar.txt
  $ hg commit -mc
  created new head
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-list.py
  > EOF
  $ hg merge 1
  U bar.txt
  U foo.txt
  merging bar.txt
  merging foo.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  warning: conflicts while merging foo.txt! (edit, then use 'hg resolve --mark')
  0 files updated, 0 files merged, 0 files removed, 2 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
  $ hg debugmergestate | grep 'merge driver:'
  merge driver: python:$TESTTMP/mergedriver-list.py (state "s")
  $ hg resolve --list
  U bar.txt
  U foo.txt
  $ hg resolve --all --tool internal:local
  (no more unresolved files)
  $ hg commit -m merge

merge driver that always takes other versions
(rc = 0, unresolved = n, driver-resolved = n)

  $ cat > ../mergedriver-other.py << EOF
  > from mercurial import debugcommands
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     overrides = {('ui', 'forcemerge'): ':other'}
  >     with ui.configoverride(overrides, 'mergedriver'):
  >         ui.setconfig('ui', 'forcemerge', ':other', 'mergedriver')
  >         mergestate.preresolve('foo.txt', wctx)
  >         mergestate.resolve('foo.txt', wctx)
  >         mergestate.preresolve('bar.txt', wctx)
  >         mergestate.resolve('bar.txt', wctx)
  >         mergestate.commit()
  > 
  >     return debugcommands.debugmergestate(ui, repo)
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     pass
  > EOF

  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-other.py
  > EOF
  $ hg up --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg merge 1
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-other.py (state "s")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "r", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "r", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  0 files updated, 2 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)

mark a file driver-resolved, and leave others unresolved
(r = False, unresolved = y, driver-resolved = y)

  $ cat > ../mergedriver-auto1.py << EOF
  > from mercurial import util
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* preprocess called\n')
  >     mergestate.mark('foo.txt', 'd')
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* conclude called\n')
  >     util.copyfile(repo.wjoin('bar.txt'), repo.wjoin('foo.txt'))
  >     mergestate.mark('foo.txt', 'r')
  > EOF
  $ hg up --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver=python:$TESTTMP/mergedriver-auto1.py
  > EOF
  $ hg merge 1
  * preprocess called
  merging bar.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  0 files updated, 0 files merged, 0 files removed, 1 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
  $ hg summary
  parent: 2:ede3d67b8d0f 
   c
  parent: 1:e0cfe070a2bb 
   b
  branch: default
  commit: 2 modified, 2 unknown, 1 unresolved (merge)
  update: 1 new changesets (update)
  phases: 4 draft
  $ hg resolve --list
  U bar.txt
  D foo.txt
  $ hg debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-auto1.py (state "m")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "u", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "D", state "d", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg resolve bar.txt --tool internal:local
  (no more unresolved files -- run "hg resolve --all" to conclude)
  $ hg resolve --list
  R bar.txt
  D foo.txt
  $ hg debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-auto1.py (state "m")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "r", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "D", state "d", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)

  $ hg resolve --all
  * conclude called
  (no more unresolved files)
  $ hg resolve --list
  R bar.txt
  R foo.txt
  $ hg commit -m 'merged'
  created new head
  $ cat foo.txt
  abar
  cbar

mark a file driver-resolved, and leave others unresolved (but skip merge driver)
(r = False, unresolved = y, driver-resolved = y)
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg merge 1
  * preprocess called
  merging bar.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  0 files updated, 0 files merged, 0 files removed, 1 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
  $ hg resolve --list
  U bar.txt
  D foo.txt
  $ hg debugmergestate | grep 'merge driver:'
  merge driver: python:$TESTTMP/mergedriver-auto1.py (state "m")
  $ hg resolve --all --skip
  warning: skipping merge driver (you MUST regenerate artifacts afterwards)
  merging bar.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  [1]
  $ hg resolve --list
  U bar.txt
  U foo.txt
  $ hg debugmergestate | grep 'merge driver:'
  [1]
  $ hg resolve --mark --all
  (no more unresolved files)
  $ hg debugmergestate | grep 'merge driver:'
  merge driver: python:$TESTTMP/mergedriver-auto1.py (state "s")
  $ hg commit -m 'merged'
  created new head

leave no files unresolved, but files driver-resolved
(r = False, unresolved = n, driver-resolved = y)

for the conclude step, also test that leaving files as driver-resolved
implicitly makes them resolved
  $ cat > ../mergedriver-driveronly.py << EOF
  > from mercurial import debugcommands
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* preprocess called\n')
  >     mergestate.mark('foo.txt', 'd')
  >     mergestate.mark('bar.txt', 'd')
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* conclude called\n')
  >     debugcommands.debugmergestate(ui, repo)
  >     mergestate.mark('foo.txt', 'r')
  > EOF
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-driveronly.py
  > EOF
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg merge 1
  * preprocess called
  * conclude called
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-driveronly.py (state "m")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "D", state "d", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "D", state "d", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ hg debugmergestate | grep 'merge driver:'
  merge driver: python:$TESTTMP/mergedriver-driveronly.py (state "s")
  $ hg commit -m 'merged'
  created new head

indicate merge driver is necessary at commit
(r = True, unresolved = n, driver-resolved = n)

  $ cat > ../mergedriver-special.py << EOF
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* preprocess called\n')
  >     overrides = {('ui', 'forcemerge'): ':other'}
  >     with ui.configoverride(overrides, 'mergedriver'):
  >         mergestate.preresolve('foo.txt', wctx)
  >         mergestate.resolve('foo.txt', wctx)
  >         mergestate.preresolve('bar.txt', wctx)
  >         mergestate.resolve('bar.txt', wctx)
  >         mergestate.commit()
  > 
  >     return True
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* conclude called\n')
  >     pass
  > EOF
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-special.py
  > EOF
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
XXX shouldn't output a warning
  $ hg merge 1
  * preprocess called
  warning: preprocess hook failed
  * conclude called
  0 files updated, 2 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ hg debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-special.py (state "s")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "r", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "r", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg commit -m 'merged'
  created new head

make sure this works sensibly when files are unresolved
(r = True, unresolved = y, driver-resolved = n)

  $ cat > ../mergedriver-exit.py << EOF
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* preprocess called\n')
  >     return True
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* conclude called\n')
  >     return True
  > EOF
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-exit.py
  > EOF
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
XXX shouldn't output a warning
  $ hg merge 1
  * preprocess called
  warning: preprocess hook failed
  merging bar.txt
  merging foo.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  warning: conflicts while merging foo.txt! (edit, then use 'hg resolve --mark')
  0 files updated, 0 files merged, 0 files removed, 2 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
  $ hg debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-exit.py (state "m")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "u", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "u", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg commit -m 'merged'
  abort: unresolved merge conflicts (see 'hg help resolve')
  [255]
  $ hg update 2
  abort: outstanding uncommitted merge
  [255]

raise an error

  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat > ../mergedriver-mark-and-raise.py << EOF
  > from mercurial import error
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* preprocess called\n')
  >     for f in mergestate:
  >         mergestate.mark(f, 'r')
  >     mergestate.commit()
  >     raise error.Abort('foo')
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* conclude called\n')
  >     raise error.Abort('bar')
  > EOF
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-mark-and-raise.py
  > EOF

  $ hg merge 1
  * preprocess called
  error: preprocess hook failed: foo
  (run with --traceback for stack trace)
  warning: merge driver failed to preprocess files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  0 files updated, 0 files merged, 0 files removed, 1 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
  $ hg debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-mark-and-raise.py (state "u")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "r", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "r", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg commit -m 'merged'
  abort: driver-resolved merge conflicts
  (run "hg resolve --all" to resolve)
  [255]
  $ hg resolve --list
  R bar.txt
  R foo.txt

this shouldn't abort
  $ hg resolve --unmark --all
  * preprocess called
  error: preprocess hook failed: foo
  (run with --traceback for stack trace)
  warning: merge driver failed to preprocess files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  $ hg resolve --list
  U bar.txt
  U foo.txt

  $ hg resolve --mark --all --skip
  warning: skipping merge driver (you MUST regenerate artifacts afterwards)
  (no more unresolved files)
  $ hg debugmergestate | grep 'merge driver:'
  [1]

subsequent resolves shouldn't trigger the merge driver at all
  $ hg resolve --unmark --all
  $ hg resolve --mark --all
  (no more unresolved files)
  $ hg debugmergestate | grep 'merge driver:'
  merge driver: python:$TESTTMP/mergedriver-mark-and-raise.py (state "s")

this should go through at this point
  $ hg commit -m merged

  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved

  $ hg merge 1
  * preprocess called
  error: preprocess hook failed: foo
  (run with --traceback for stack trace)
  warning: merge driver failed to preprocess files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  0 files updated, 0 files merged, 0 files removed, 1 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]

XXX this is really confused
  $ hg resolve --mark --all
  * preprocess called
  error: preprocess hook failed: foo
  (run with --traceback for stack trace)
  warning: merge driver failed to preprocess files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  * conclude called
  error: conclude hook failed: bar
  (run with --traceback for stack trace)
  warning: merge driver failed to resolve files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  [1]
  $ hg resolve --list
  R bar.txt
  R foo.txt

this should abort
  $ hg commit -m 'merged'
  abort: driver-resolved merge conflicts
  (run "hg resolve --all" to resolve)
  [255]

this should disable the merge driver
  $ hg help resolve | grep -- 'skip'
      --skip                skip merge driver
  $ hg resolve --all --skip
  warning: skipping merge driver (you MUST regenerate artifacts afterwards)
  (no more unresolved files)

this should go through
  $ hg commit -m merged

this shouldn't invoke the merge driver
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved

nor should this no-op update
  $ hg update 2
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved

nor should this update with no working copy changes
  $ hg update 1
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved

test some other failure modes

  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved

  $ hg merge 1 --config experimental.mergedriver=fail
  abort: merge driver must be a python hook
  [255]
  $ hg update --clean 2
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
this should proceed as if there's no merge driver
  $ hg merge 1 --config experimental.mergedriver=python:fail
  loading preprocess hook failed:
  merging bar.txt
  merging foo.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  warning: conflicts while merging foo.txt! (edit, then use 'hg resolve --mark')
  0 files updated, 0 files merged, 0 files removed, 2 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
  $ hg debugmergestate | grep 'merge driver:'
  merge driver: python:fail (state "s")
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd ..
ensure the right path to load the merge driver hook
  $ hg -R repo1 merge 1 --config experimental.mergedriver=python:fail
  loading preprocess hook failed:
  merging bar.txt
  merging foo.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  warning: conflicts while merging foo.txt! (edit, then use 'hg resolve --mark')
  0 files updated, 0 files merged, 0 files removed, 2 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon
  [1]
verify behavior with different merge driver
  $ hg -R repo1 debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:fail (state "s")
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "u", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "u", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg -R repo1 resolve --mark --all --config experimental.mergedriver=
  (no more unresolved files)
  $ hg -R repo1 debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  labels:
    local: working copy
    other: merge rev
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "r", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "r", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg -R repo1 commit -m merged
  created new head

this should invoke the merge driver
  $ cd repo1
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat > ../mergedriver-raise.py << EOF
  > from mercurial import error
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* preprocess called\n')
  >     raise error.Abort('foo')
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     repo.ui.status('* conclude called\n')
  >     raise error.Abort('bar')
  > EOF
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-raise.py
  > EOF
  $ echo foowd >> foo.txt
  $ hg update ".^"
  * preprocess called
  error: preprocess hook failed: foo
  (run with --traceback for stack trace)
  warning: merge driver failed to preprocess files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  1 files updated, 0 files merged, 0 files removed, 1 files unresolved
  use 'hg resolve' to retry unresolved file merges
  [1]
  $ hg debugmergestate
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92
  merge driver: python:$TESTTMP/mergedriver-raise.py (state "u")
  labels:
    local: working copy
    other: destination
  file extras: foo.txt (ancestorlinknode = ede3d67b8d0fb0052854c85fb16823c825d21060)
  file: foo.txt (record type "F", state "u", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node 802224e80e899817a159d494c123fb421ac3efee)
    other path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
  $ hg resolve --list
  U foo.txt
XXX this is really confused
  $ hg resolve --mark --all
  * preprocess called
  error: preprocess hook failed: foo
  (run with --traceback for stack trace)
  warning: merge driver failed to preprocess files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  * conclude called
  error: conclude hook failed: bar
  (run with --traceback for stack trace)
  warning: merge driver failed to resolve files
  (hg resolve --all to retry, or hg resolve --all --skip to skip merge driver)
  [1]

test merge with automatic commit afterwards -- e.g. graft

  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-other.py
  > EOF
  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg debugmergestate
  no merge state found
  $ hg graft 1
  grafting 1:e0cfe070a2bb "b"
  * version 2 records
  local: ede3d67b8d0fb0052854c85fb16823c825d21060
  other: e0cfe070a2bbd0b727903026b7026cb0917e63b3
  merge driver: python:$TESTTMP/mergedriver-other.py (state "s")
  labels:
    local: local
    other: graft
  file extras: bar.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: bar.txt (record type "F", state "r", hash 9d6caa30f54d05af0edb194bfa26137b109f2112)
    local path: bar.txt (flags "")
    ancestor path: bar.txt (node 4f30a68d92d62ca460d2c484d3fe4584c0521ae1)
    other path: bar.txt (node 18db82bb5e3b439444a63baf35364169e848cfd2)
  file extras: foo.txt (ancestorlinknode = b9c4506f0639a99fcbfb8ce4764aa2aa4d2f6f92)
  file: foo.txt (record type "F", state "r", hash 9206ac42b532ef8e983470c251f4e1a365fd636c)
    local path: foo.txt (flags "")
    ancestor path: foo.txt (node ad59c7ac23656632da079904d4d40d0bab4aeb80)
    other path: foo.txt (node 0b0743b512ba9b7c5db411597cf374a73b9f00ac)
  $ hg export
  # HG changeset patch
  # User test
  # Date 0 0
  #      Thu Jan 01 00:00:00 1970 +0000
  # Node ID 87ae466e19391befaaa0b92212ad70eef907404a
  # Parent  ede3d67b8d0fb0052854c85fb16823c825d21060
  b
  
  diff -r ede3d67b8d0f -r 87ae466e1939 bar.txt
  --- a/bar.txt	Thu Jan 01 00:00:00 1970 +0000
  +++ b/bar.txt	Thu Jan 01 00:00:00 1970 +0000
  @@ -1,2 +1,2 @@
   abar
  -cbar
  +bbar
  diff -r ede3d67b8d0f -r 87ae466e1939 foo.txt
  --- a/foo.txt	Thu Jan 01 00:00:00 1970 +0000
  +++ b/foo.txt	Thu Jan 01 00:00:00 1970 +0000
  @@ -1,2 +1,2 @@
   afoo
  -cfoo
  +bfoo

graft with failing merge

  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-auto1.py
  > EOF
  $ hg graft 1
  grafting 1:e0cfe070a2bb "b"
  * preprocess called
  merging bar.txt
  warning: conflicts while merging bar.txt! (edit, then use 'hg resolve --mark')
  abort: unresolved conflicts, can't continue
  (use 'hg resolve' and 'hg graft --continue')
  [255]
  $ hg resolve --list
  U bar.txt
  D foo.txt
  $ hg resolve --mark bar.txt
  (no more unresolved files -- run "hg resolve --all" to conclude)
  $ hg graft --continue
  grafting 1:e0cfe070a2bb "b"
  abort: driver-resolved merge conflicts
  (run "hg resolve --all" to resolve)
  [255]
  $ hg resolve --unmark bar.txt
  $ hg resolve --list
  U bar.txt
  D foo.txt
  $ hg resolve foo.txt bar.txt --tool :other
  * conclude called
  (no more unresolved files)
  continue: hg graft --continue
XXX hg resolve --unmark --all doesn't cause the merge driver to be rerun
  $ hg resolve --mark --all
  (no more unresolved files)
  continue: hg graft --continue
  $ hg graft --continue
  grafting 1:e0cfe070a2bb "b"
  $ hg export
  # HG changeset patch
  # User test
  # Date 0 0
  #      Thu Jan 01 00:00:00 1970 +0000
  # Node ID f22dab3f2e1c58088986931026ef5c22ba3f4006
  # Parent  ede3d67b8d0fb0052854c85fb16823c825d21060
  b
  
  diff -r ede3d67b8d0f -r f22dab3f2e1c bar.txt
  --- a/bar.txt	Thu Jan 01 00:00:00 1970 +0000
  +++ b/bar.txt	Thu Jan 01 00:00:00 1970 +0000
  @@ -1,2 +1,2 @@
   abar
  -cbar
  +bbar
  diff -r ede3d67b8d0f -r f22dab3f2e1c foo.txt
  --- a/foo.txt	Thu Jan 01 00:00:00 1970 +0000
  +++ b/foo.txt	Thu Jan 01 00:00:00 1970 +0000
  @@ -1,2 +1,2 @@
  -afoo
  -cfoo
  +abar
  +bbar

delete all the files

  $ hg update --clean 2
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cat > ../mergedriver-delete.py << EOF
  > import os
  > def preprocess(ui, repo, hooktype, mergestate, wctx, labels):
  >     ui.status('* preprocess called\n')
  >     for f in mergestate:
  >         mergestate.mark(f, 'd')
  > def conclude(ui, repo, hooktype, mergestate, wctx, labels):
  >     ui.status('* conclude called\n')
  >     for f in mergestate.driverresolved():
  >         os.unlink(f)
  >         mergestate.queueremove(f)
  > EOF
  $ cat >> $HGRCPATH << EOF
  > [experimental]
  > mergedriver = python:$TESTTMP/mergedriver-delete.py
  > EOF
  $ hg graft 1
  grafting 1:e0cfe070a2bb "b"
  * preprocess called
  * conclude called
  $ hg status --change .
  R bar.txt
  R foo.txt
  $ f foo.txt bar.txt
  bar.txt: file not found
  foo.txt: file not found
  $ hg files
  [1]
