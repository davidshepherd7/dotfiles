  $ PYTHONPATH=$TESTDIR/..:$PYTHONPATH
  $ export PYTHONPATH

  $ cat <<EOF >> $HGRCPATH
  > [extensions]
  > mq =
  > obsshelve=$TESTDIR/../hgext3rd/obsshelve.py
  > [defaults]
  > diff = --nodates --git
  > qnew = --date '0 0'
  > [shelve]
  > maxbackups = 2
  > [experimental]
  > evolution=createmarkers
  > EOF

Make sure obs-based shelve can be used with an empty repo
  $ cd $TESTTMP
  $ hg init obsrepo
  $ cd obsrepo
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > obsshelve=True
  > EOF

  $ mkdir a b
  $ echo a > a/a
  $ echo b > b/b
  $ echo c > c
  $ echo d > d
  $ echo x > x
  $ hg addremove -q
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 5 files removed, 0 files unresolved
  $ hg shelve --list
  default         (*s ago)    (changes in empty repository) (glob)
  $ hg revert --all
  $ hg unshelve
  unshelving change 'default'
  $ hg diff
  diff --git a/a/a b/a/a
  new file mode 100644
  --- /dev/null
  +++ b/a/a
  @@ -0,0 +1,1 @@
  +a
  diff --git a/b/b b/b/b
  new file mode 100644
  --- /dev/null
  +++ b/b/b
  @@ -0,0 +1,1 @@
  +b
  diff --git a/c b/c
  new file mode 100644
  --- /dev/null
  +++ b/c
  @@ -0,0 +1,1 @@
  +c
  diff --git a/d b/d
  new file mode 100644
  --- /dev/null
  +++ b/d
  @@ -0,0 +1,1 @@
  +d
  diff --git a/x b/x
  new file mode 100644
  --- /dev/null
  +++ b/x
  @@ -0,0 +1,1 @@
  +x
  $ hg ci -qm "initial commit"
  $ hg shelve
  nothing changed
  [1]

Make sure shelve files were backed up
  $ ls .hg/shelve-backup
  default.oshelve
  default.patch

Create an mq patch - shelving should work fine with a patch applied
  $ echo n > n
  $ hg add n
  $ hg commit n -m second
  $ hg qnew second.patch

Shelve a change that we will delete later
  $ echo a >> a/a
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

Set up some more complex changes to shelve
  $ echo a >> a/a
  $ hg mv b b.rename
  moving b/b to b.rename/b (glob)
  $ hg cp c c.copy
  $ hg status -C
  M a/a
  A b.rename/b
    b/b
  A c.copy
    c
  R b/b

The common case - no options or filenames
  $ hg shelve
  shelved as default-01
  2 files updated, 0 files merged, 2 files removed, 0 files unresolved
  $ hg status -C

Ensure that our shelved changes exist
  $ hg shelve -l
  default-01      (*)* changes to: [mq]: second.patch (glob)
  default         (*)* changes to: [mq]: second.patch (glob)
  $ hg shelve -l -p default
  default         (*)* changes to: [mq]: second.patch (glob)
  
  diff --git a/a/a b/a/a
  --- a/a/a
  +++ b/a/a
  @@ -1,1 +1,2 @@
   a
  +a

  $ hg shelve --list --addremove
  abort: options '--list' and '--addremove' may not be used together
  [255]

Delete our older shelved change
  $ hg shelve -d default
  $ hg qfinish -a -q

Ensure shelve backups aren't overwritten
  $ ls .hg/shelve-backup/
  default-1.oshelve
  default-1.patch
  default.oshelve
  default.patch

Local edits should not prevent a shelved change from applying
  $ printf "z\na\n" > a/a
  $ hg unshelve --keep
  unshelving change 'default-01'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 5:32c69314e062 "changes to: [mq]: second.patch"
  merging a/a

  $ hg revert --all -q
  $ rm a/a.orig b.rename/b c.copy

Apply it and make sure our state is as expected
(this also tests that same timestamp prevents backups from being
removed, even though there are more than 'maxbackups' backups)
  $ f -t .hg/shelve-backup/default.patch
  .hg/shelve-backup/default.patch: file
  $ touch -t 200001010000 .hg/shelve-backup/default.patch
  $ f -t .hg/shelve-backup/default-1.patch
  .hg/shelve-backup/default-1.patch: file
  $ touch -t 200001010000 .hg/shelve-backup/default-1.patch

  $ hg unshelve
  unshelving change 'default-01'
  $ hg status -C
  M a/a
  A b.rename/b
    b/b
  A c.copy
    c
  R b/b
  $ hg shelve -l

(both of default.oshelve and default-1.oshelve should be still kept,
because it is difficult to decide actual order of them from same timestamp)
  $ ls .hg/shelve-backup/
  default-01.oshelve
  default-01.patch
  default-1.oshelve
  default-1.patch
  default.oshelve
  default.patch
  $ hg unshelve
  abort: no shelved changes to apply!
  [255]
  $ hg unshelve foo
  abort: shelved change 'foo' not found
  [255]

Named shelves, specific filenames, and "commit messages" should all work
(this tests also that editor is invoked, if '--edit' is specified)
  $ hg status -C
  M a/a
  A b.rename/b
    b/b
  A c.copy
    c
  R b/b
  $ HGEDITOR=cat hg shelve -q -n wibble -m wat -e a
  wat
  
  
  HG: Enter commit message.  Lines beginning with 'HG:' are removed.
  HG: Leave message empty to abort commit.
  HG: --
  HG: user: shelve@localhost
  HG: branch 'default'
  HG: changed a/a

Expect "a" to no longer be present, but status otherwise unchanged
  $ hg status -C
  A b.rename/b
    b/b
  A c.copy
    c
  R b/b
  $ hg shelve -l --stat
  wibble          (*)    wat (glob)
   a/a |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)

And now "a/a" should reappear
  $ cd a
  $ hg unshelve -q wibble
  $ cd ..
  $ hg status -C
  M a/a
  A b.rename/b
    b/b
  A c.copy
    c
  R b/b

Ensure old shelve backups are being deleted automatically
  $ ls .hg/shelve-backup/
  default-01.oshelve
  default-01.patch
  wibble.oshelve
  wibble.patch

Cause unshelving to result in a merge with 'a' conflicting
  $ hg shelve -q
  $ echo c>>a/a
  $ hg commit -m second
  $ hg tip --template '{files}\n'
  a/a

Add an unrelated change that should be preserved
  $ mkdir foo
  $ echo foo > foo/foo
  $ hg add foo/foo

Force a conflicted merge to occur
  $ hg unshelve
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 5:32c69314e062 "changes to: [mq]: second.patch"
  merging a/a
  warning: conflicts while merging a/a! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]

Ensure that we have a merge with unresolved conflicts
  $ hg heads -q --template '{rev}\n'
  12
  5
  $ hg parents -q --template '{rev}\n'
  12
  5
  $ hg status
  M a/a
  M b.rename/b
  M c.copy
  R b/b
  ? a/a.orig
  $ hg diff
  diff --git a/a/a b/a/a
  --- a/a/a
  +++ b/a/a
  @@ -1,2 +1,6 @@
   a
  +<<<<<<< dest:   * - shelve: pending changes temporary commit (glob)
   c
  +=======
  +a
  +>>>>>>> source: 32c69314e062 - shelve: changes to: [mq]: second.patch
  diff --git a/b/b b/b.rename/b
  rename from b/b
  rename to b.rename/b
  diff --git a/c b/c.copy
  copy from c
  copy to c.copy
  $ hg resolve -l
  U a/a

  $ hg shelve
  abort: unshelve already in progress
  (use 'hg unshelve --continue' or 'hg unshelve --abort')
  [255]

Abort the unshelve and be happy
  $ hg status
  M a/a
  M b.rename/b
  M c.copy
  R b/b
  ? a/a.orig
  $ hg unshelve -a
  rebase aborted
  unshelve of 'default' aborted
  $ hg heads -q
  11:2e69b451d1ea
  $ hg parents | grep changeset
  changeset:   11:2e69b451d1ea
  $ hg resolve -l
  $ hg status
  A foo/foo
  ? a/a.orig

Try to continue with no unshelve underway
  $ hg unshelve -c
  abort: no unshelve in progress
  [255]
  $ hg status
  A foo/foo
  ? a/a.orig

Redo the unshelve to get a conflict
  $ hg unshelve -q
  warning: conflicts while merging a/a! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]

Attempt to continue
  $ hg unshelve -c
  abort: unresolved conflicts, can't continue
  (see 'hg resolve', then 'hg unshelve --continue')
  [255]
  $ hg revert -r . a/a
  $ hg resolve -m a/a
  (no more unresolved files)
  continue: hg unshelve --continue
  $ hg commit -m 'commit while unshelve in progress'
  abort: unshelve already in progress
  (use 'hg unshelve --continue' or 'hg unshelve --abort')
  [255]
  $ hg graft --continue
  abort: no graft in progress
  (continue: hg unshelve --continue)
  [255]
  $ hg unshelve -c --trace
  rebasing 5:32c69314e062 "changes to: [mq]: second.patch"
  unshelve of 'default' complete

Ensure the repo is as we hope
  $ hg parents | grep changeset
  changeset:   11:2e69b451d1ea
  $ hg heads -q
  11:2e69b451d1ea
  $ hg status -C
  A b.rename/b
    b/b
  A c.copy
    c
  A foo/foo
  R b/b
  ? a/a.orig

There should be no shelves left
  $ hg shelve -l

#if execbit
Ensure that metadata-only changes are shelved
  $ chmod +x a/a
  $ hg shelve -q -n execbit a/a
  $ hg status a/a
  $ hg unshelve -q execbit
  $ hg status a/a
  M a/a
  $ hg revert a/a
#endif

#if symlink
Ensure symlinks are properly handled
  $ rm a/a
  $ ln -s foo a/a
  $ hg shelve -q -n symlink a/a
  $ hg status a/a
  $ hg unshelve -q symlink
  $ hg status a/a
  M a/a
  $ hg revert a/a
#endif

Set up another conflict between a commit and a shelved change
  $ hg revert -q -C -a
  $ rm a/a.orig b.rename/b c.copy
  $ echo a >> a/a
  $ hg shelve -q
  $ echo x >> a/a
  $ hg ci -m 'create conflict'
  $ hg add foo/foo

If we resolve a conflict while unshelving, the unshelve should succeed
  $ hg unshelve --tool :merge-other --keep
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing .* "changes to: second" (re)
  merging a/a
  $ hg shelve -l
  default         (*)* changes to: second (glob)
  $ hg status
  M a/a
  A foo/foo
  $ cat a/a
  a
  c
  a
  $ cat > a/a << EOF
  > a
  > c
  > x
  > EOF
  $ sleep 1
  $ HGMERGE=true hg unshelve
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing .* "changes to: second" (re)
  merging a/a
  note: rebase of .* created no changes to commit (re)
  $ hg shelve -l
  $ hg status
  A foo/foo
  $ cat a/a
  a
  c
  x

Test keep and cleanup
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg shelve --list
  default         (*)* changes to: create conflict (glob)
  $ hg unshelve -k
  unshelving change 'default'
  $ hg shelve --list
  default         (*)* changes to: create conflict (glob)
  $ hg shelve --cleanup
  $ hg shelve --list

Test bookmarks
  $ hg bookmark test
  $ hg bookmark
   \* test                      * (glob)
  $ hg shelve
  shelved as test
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg bookmark
   \* test                      * (glob)
  $ hg unshelve
  unshelving change 'test'
  $ hg bookmark
   \* test                      * (glob)

Shelve should still work even if mq is disabled
  $ hg --config extensions.mq=! shelve
  shelved as test
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg --config extensions.mq=! shelve --list
  test            (*)* changes to: create conflict (glob)
  $ hg bookmark
   * test                      * (glob)
  $ hg --config extensions.mq=! unshelve
  unshelving change 'test'
  $ hg bookmark
   * test                      * (glob)
  $ cd ..

Shelve should leave dirstate clean (issue4055)
  $ hg init obsshelverebase
  $ cd obsshelverebase
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ printf 'x\ny\n' > x
  $ echo z > z
  $ hg commit -Aqm xy
  $ echo z >> x
  $ hg commit -Aqm z
  $ hg up 0
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ printf 'a\nx\ny\nz\n' > x
  $ hg commit -Aqm xyz
  $ echo c >> z
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg rebase -d 1 --config extensions.rebase=
  rebasing 2:323bfa07f744 "xyz" (tip)
  merging x
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing 3:82a0d7d6ba61 "changes to: xyz"
  $ hg status
  M z
  $ cd ..

Shelve should only unshelve pending changes (issue4068)
  $ hg init obssh-onlypendingchanges
  $ cd obssh-onlypendingchanges
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ touch a
  $ hg ci -Aqm a
  $ touch b
  $ hg ci -Aqm b
  $ hg up -q 0
  $ touch c
  $ hg ci -Aqm c
  $ touch d
  $ hg add d
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg up -q 1
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing 3:958bcbd1776e "changes to: c" (tip)
  $ hg status
  A d

Unshelve should work on an ancestor of the original commit
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg up 0
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing 5:013284d9655e "changes to: b" (tip)
  $ hg status
  A d

Test bug 4073 we need to enable obsolete markers for it
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg debugobsolete `hg --debug id -i -r 1`
  obsoleted 1 changesets
  $ hg unshelve
  unshelving change 'default'

Unshelve should leave unknown files alone (issue4113)
  $ echo e > e
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg status
  ? e
  $ hg unshelve
  unshelving change 'default'
  $ hg status
  A d
  ? e
  $ cat e
  e

139. Unshelve should keep a copy of unknown files

  $ hg add e
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 2 files removed, 0 files unresolved
  $ echo z > e
  $ hg unshelve
  unshelving change 'default'
  $ cat e
  e
  $ cat e.orig
  z

140. Unshelve and conflicts with tracked and untracked files

 preparing:

  $ rm *.orig
  $ hg ci -qm 'commit stuff'
  $ hg phase -p null:

 no other changes - no merge:

  $ echo f > f
  $ hg add f
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ echo g > f
  $ hg unshelve
  unshelving change 'default'
  $ hg st
  A f
  ? f.orig
  $ cat f
  f
  $ cat f.orig
  g

 other uncommitted changes - merge:

  $ hg st
  A f
  ? f.orig
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg log -G --template '{rev}  {desc|firstline}  {author}'
  @  9  commit stuff  test
  |
  | o  2  c  test
  |/
  o  0  a  test
  
  $ mv f.orig f
  $ echo 1 > a
  $ hg unshelve --date '1073741824 0'
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 10:81152db69da7 "changes to: commit stuff"
  merging f
  warning: conflicts while merging f! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ hg parents -T "{desc|firstline}\n" | sort
  changes to: commit stuff
  pending changes temporary commit

  $ hg st
  M f
  ? f.orig
  $ cat f
  <<<<<<< dest:   5f6b880e719b - shelve: pending changes temporary commit
  g
  =======
  f
  >>>>>>> source: 81152db69da7 - shelve: changes to: commit stuff
  $ cat f.orig
  g
  $ hg unshelve --abort -t false
  tool option will be ignored
  rebase aborted
  unshelve of 'default' aborted
  $ hg st
  M a
  ? f.orig
  $ cat f.orig
  g
  $ hg unshelve
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 10:81152db69da7 "changes to: commit stuff"
  $ hg st
  M a
  A f
  ? f.orig

 other committed changes - merge:

  $ hg shelve f
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg ci a -m 'intermediate other change'
  $ mv f.orig f
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing 10:81152db69da7 "changes to: commit stuff"
  merging f
  warning: conflicts while merging f! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ hg st
  M f
  ? f.orig
  $ cat f
  <<<<<<< dest:   * - test: intermediate other change (glob)
  g
  =======
  f
  >>>>>>> source: 81152db69da7 - shelve: changes to: commit stuff
  $ cat f.orig
  g
  $ hg unshelve --abort
  rebase aborted
  unshelve of 'default' aborted
  $ hg st
  ? f.orig
  $ cat f.orig
  g
  $ hg shelve --delete default

Recreate some conflict again
  $ cd ../obsrepo
  $ hg up -C -r 'test^'
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (leaving bookmark test)
  $ echo y >> a/a
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg up test
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (activating bookmark test)
  $ hg bookmark
   * test                      * (glob)
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing * "changes to: second" (tip) (glob)
  merging a/a
  warning: conflicts while merging a/a! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ hg bookmark
     test                      * (glob)

Test that resolving all conflicts in one direction (so that the rebase
is a no-op), works (issue4398)

  $ hg revert -a -r .
  reverting a/a (glob)
  $ hg resolve -m a/a
  (no more unresolved files)
  continue: hg unshelve --continue
  $ hg unshelve -c
  rebasing * "changes to: second" (tip) (glob)
  note: rebase of * created no changes to commit (glob)
  unshelve of 'default' complete
  $ hg bookmark
   * test                      * (glob)
  $ hg diff
  $ hg status
  ? a/a.orig
  ? foo/foo
  $ hg summary | egrep "(bookmarks|commit)"
  bookmarks: *test
  commit: 2 unknown (clean)

  $ hg shelve --delete --stat
  abort: options '--delete' and '--stat' may not be used together
  [255]
  $ hg shelve --delete --name NAME
  abort: options '--delete' and '--name' may not be used together
  [255]

Test interactive shelve
  $ cat <<EOF >> $HGRCPATH
  > [ui]
  > interactive = true
  > EOF
  $ echo 'a' >> a/b
  $ cat a/a >> a/b
  $ echo 'x' >> a/b
  $ mv a/b a/a
  $ echo 'a' >> foo/foo
  $ hg st
  M a/a
  ? a/a.orig
  ? foo/foo
  $ cat a/a
  a
  a
  c
  x
  x
  $ cat foo/foo
  foo
  a
  $ hg shelve --interactive --config ui.interactive=false
  abort: running non-interactively
  [255]
  $ hg shelve --interactive << EOF
  > y
  > y
  > n
  > EOF
  diff --git a/a/a b/a/a
  2 hunks, 2 lines changed
  examine changes to 'a/a'? [Ynesfdaq?] y
  
  @@ -1,3 +1,4 @@
  +a
   a
   c
   x
  record change 1/2 to 'a/a'? [Ynesfdaq?] y
  
  @@ -1,3 +2,4 @@
   a
   c
   x
  +x
  record change 2/2 to 'a/a'? [Ynesfdaq?] n
  
  shelved as test
  merging a/a
  0 files updated, 1 files merged, 0 files removed, 0 files unresolved
  $ cat a/a
  a
  c
  x
  x
  $ cat foo/foo
  foo
  a
  $ hg st
  M a/a
  ? foo/foo
  $ hg bookmark
   * test                      * (glob)
  $ hg log -r . -T "{desc|firstline}\n"
  create conflict
  $ hg unshelve
  unshelving change 'test'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing * "changes to: create conflict" (glob)
  merging a/a
  $ hg bookmark
   * test                      * (glob)
  $ hg log -r . -T "{desc|firstline}\n"
  create conflict
  $ cat a/a
  a
  a
  c
  x
  x

Shelve --patch and shelve --stat should work with a single valid shelfname
  $ hg up --clean .
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (leaving bookmark test)
  $ hg shelve --list
  $ echo 'patch a' > shelf-patch-a
  $ hg add shelf-patch-a
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ echo 'patch b' > shelf-patch-b
  $ hg add shelf-patch-b
  $ hg shelve
  shelved as default-01
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg shelve --patch default default-01
  default-01      (*s ago)    changes to: create conflict (glob)
  
  diff --git a/shelf-patch-b b/shelf-patch-b
  new file mode 100644
  --- /dev/null
  +++ b/shelf-patch-b
  @@ -0,0 +1,1 @@
  +patch b
  default         (*s ago)    changes to: create conflict (glob)
  
  diff --git a/shelf-patch-a b/shelf-patch-a
  new file mode 100644
  --- /dev/null
  +++ b/shelf-patch-a
  @@ -0,0 +1,1 @@
  +patch a
  $ hg shelve --stat default default-01
  default-01      (*s ago)    changes to: create conflict (glob)
   shelf-patch-b |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)
  default         (*s ago)    changes to: create conflict (glob)
   shelf-patch-a |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)
  $ hg shelve --patch default
  default         (*)* changes to: create conflict (glob)
  
  diff --git a/shelf-patch-a b/shelf-patch-a
  new file mode 100644
  --- /dev/null
  +++ b/shelf-patch-a
  @@ -0,0 +1,1 @@
  +patch a
  $ hg shelve --stat default
  default         (*)* changes to: create conflict (glob)
   shelf-patch-a |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)
  $ hg shelve --patch nonexistentshelf
  abort: cannot find shelf nonexistentshelf
  [255]
  $ hg shelve --stat nonexistentshelf
  abort: cannot find shelf nonexistentshelf
  [255]

Test visibility of in-memory changes inside transaction to external hook
------------------------------------------------------------------------
  $ echo xxxx >> x
  $ hg commit -m "changes to invoke rebase"
  $ hg bookmark unshelvedest

  $ cat > $TESTTMP/checkvisibility.sh <<EOF
  > echo "==== \$1:"
  > hg parents --template "VISIBLE {node|short}\n"
  > # test that pending changes are hidden
  > unset HG_PENDING
  > hg parents --template "ACTUAL  {node|short}\n"
  > echo "===="
  > EOF

  $ cat >> .hg/hgrc <<EOF
  > [defaults]
  > # to fix hash id of temporary revisions
  > unshelve = --date '0 0'
  > EOF

"hg unshelve"implies steps below:
(1) commit changes in the working directory
(2) note shelved revision
(3) rebase: merge shelved revision into temporary wc changes
(4) rebase: commit merged revision
(5) rebase: update to a new commit
(6) update to original working copy parent

== test visibility to external preupdate hook

  $ cat >> .hg/hgrc <<EOF
  > [hooks]
  > preupdate.visibility = sh $TESTTMP/checkvisibility.sh preupdate
  > EOF

  $ echo nnnn >> n

  $ sh $TESTTMP/checkvisibility.sh before-unshelving
  ==== before-unshelving:
  VISIBLE f77bf047d4c5
  ACTUAL  f77bf047d4c5
  ====

  $ hg unshelve --keep default
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing *206bf5d4f922 "changes to: create conflict" (glob)
  ==== preupdate:
  VISIBLE (?!f77bf047d4c5).* (re)
  ACTUAL  f77bf047d4c5
  ====
  ==== preupdate:
  VISIBLE (?!f77bf047d4c5).* (re)
  ACTUAL  f77bf047d4c5
  ====
  ==== preupdate:
  VISIBLE (?!f77bf047d4c5).* (re)
  ACTUAL  f77bf047d4c5
  ====

  $ cat >> .hg/hgrc <<EOF
  > [hooks]
  > preupdate.visibility =
  > EOF

  $ sh $TESTTMP/checkvisibility.sh after-unshelving
  ==== after-unshelving:
  VISIBLE f77bf047d4c5
  ACTUAL  f77bf047d4c5
  ====

== test visibility to external update hook

  $ hg update -q -C unshelvedest

  $ cat >> .hg/hgrc <<EOF
  > [hooks]
  > update.visibility = sh $TESTTMP/checkvisibility.sh update
  > EOF

  $ echo nnnn >> n

  $ sh $TESTTMP/checkvisibility.sh before-unshelving
  ==== before-unshelving:
  VISIBLE f77bf047d4c5
  ACTUAL  f77bf047d4c5
  ====

  $ hg unshelve --keep default
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing *:206bf5d4f922 "changes to: create conflict" (glob)
  ==== update:
  VISIBLE f3a8cb815d40
  VISIBLE 206bf5d4f922
  ACTUAL  f77bf047d4c5
  ====
  ==== update:
  VISIBLE f3a8cb815d40
  ACTUAL  f77bf047d4c5
  ====
  ==== update:
  VISIBLE f77bf047d4c5
  ACTUAL  f77bf047d4c5
  ====

  $ cat >> .hg/hgrc <<EOF
  > [hooks]
  > update.visibility =
  > EOF

  $ sh $TESTTMP/checkvisibility.sh after-unshelving
  ==== after-unshelving:
  VISIBLE f77bf047d4c5
  ACTUAL  f77bf047d4c5
  ====
  $ hg bookmark -d unshelvedest
  $ cd ..

Test .orig files go where the user wants them to
---------------------------------------------------------------
  $ hg init obssh-salvage
  $ cd obssh-salvage
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo 'content' > root
  $ hg commit -A -m 'root' -q
  $ echo '' > root
  $ hg shelve -q
  $ echo 'contADDent' > root
  $ hg unshelve -q --config 'ui.origbackuppath=.hg/origbackups'
  warning: conflicts while merging root! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ ls .hg/origbackups
  root.orig
  $ rm -rf .hg/origbackups

Test Abort unshelve always gets user out of the unshelved state
---------------------------------------------------------------
Wreak havoc on the unshelve process
  $ rm .hg/unshelverebasestate
  $ hg unshelve --abort
  unshelve of 'default' aborted
  abort: (No such file or directory|The system cannot find the file specified) (re)
  [255]
Can the user leave the current state?
  $ hg up -C .
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

Try again but with a corrupted shelve state file
  $ hg up -r 0 -q
  $ echo '' > root
  $ hg shelve -q
  $ echo 'contADDent' > root
  $ hg unshelve -q
  warning: conflicts while merging root! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ sed 's/ae8c668541e8/123456789012/' .hg/shelvedstate > ../corrupt-shelvedstate
  $ mv ../corrupt-shelvedstate .hg/histedit-state
  $ hg unshelve --abort 2>&1 | grep 'rebase aborted'
  rebase aborted
  $ hg up -C .
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd ..

Keep active bookmark while (un)shelving even on shared repo (issue4940)
-----------------------------------------------------------------------
  $ cat <<EOF >> $HGRCPATH
  > [extensions]
  > share =
  > [experimnetal]
  > evolution=createmarkers
  > EOF
  $ hg bookmarks -R obsrepo
     test                      *:33f7f61e6c5e (glob)
  $ hg share -B obsrepo obsshare
  updating working directory
  6 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd obsshare

  $ hg bookmarks
     test                      *:33f7f61e6c5e (glob)
  $ hg bookmarks foo
  $ hg bookmarks
   \* foo                       *:f77bf047d4c5 (glob)
     test                      *:33f7f61e6c5e (glob)
  $ echo x >> x
  $ hg shelve
  shelved as foo
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg bookmarks
   \* foo                       *:f77bf047d4c5 (glob)
     test                      *:33f7f61e6c5e (glob)

  $ hg unshelve
  unshelving change 'foo'
  $ hg bookmarks
   \* foo                       *:f77bf047d4c5 (glob)
     test                      *:33f7f61e6c5e (glob)

  $ cd ..

Shelve and unshelve unknown files. For the purposes of unshelve, a shelved
unknown file is the same as a shelved added file, except that it will be in
unknown state after unshelve if and only if it was either absent or unknown
before the unshelve operation.
  $ hg init obssh-unknowns
  $ cd obssh-unknowns
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF

The simplest case is if I simply have an unknown file that I shelve and unshelve
  $ echo unknown > unknown
  $ hg status
  ? unknown
  $ hg shelve --unknown
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg status
  $ hg unshelve
  unshelving change 'default'
  $ hg status
  ? unknown
  $ rm unknown

If I shelve, add the file, and unshelve, does it stay added?
  $ echo unknown > unknown
  $ hg shelve -u
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg status
  $ touch unknown
  $ hg add unknown
  $ hg status
  A unknown
  $ hg unshelve
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 0:098df96e7410 "(changes in empty repository)"
  merging unknown
  $ hg status
  A unknown
  $ hg forget unknown
  $ rm unknown

And if I shelve, commit, then unshelve, does it become modified?
  $ echo unknown > unknown
  $ hg shelve -u
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg status
  $ touch unknown
  $ hg add unknown
  $ hg commit -qm "Add unknown"
  $ hg status
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing 0:098df96e7410 "(changes in empty repository)"
  merging unknown
  $ hg status
  M unknown
  $ hg remove --force unknown
  $ hg commit -qm "Remove unknown"
  $ cd ..

We expects that non-bare shelve keeps newly created branch in
working directory.
  $ hg init obs-shelve-preserve-new-branch
  $ cd obs-shelve-preserve-new-branch
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo "a" >> a
  $ hg add a
  $ echo "b" >> b
  $ hg add b
  $ hg commit -m "ab"
  $ echo "aa" >> a
  $ echo "bb" >> b
  $ hg branch new-branch
  marked working directory as branch new-branch
  (branches are permanent and global, did you want a bookmark?)
  $ hg status
  M a
  M b
  $ hg branch
  new-branch
  $ hg shelve a
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch
  new-branch
  $ hg status
  M b
  $ touch "c" >> c
  $ hg add c
  $ hg status
  M b
  A c
  $ hg shelve --exclude c
  shelved as default-01
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch
  new-branch
  $ hg status
  A c
  $ hg shelve --include c
  shelved as default-02
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg branch
  new-branch
  $ hg status
  $ echo "d" >> d
  $ hg add d
  $ hg status
  A d

We expect that bare-shelve will not keep branch in current working directory.

  $ hg shelve
  shelved as default-03
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg branch
  default
  $ cd ..

When i shelve commit on newly created branch i expect
that after unshelve newly created branch will be preserved.
  $ hg init obs-shelve_on_new_branch_simple
  $ cd obs-shelve_on_new_branch_simple
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo "aaa" >> a
  $ hg commit -A -m "a"
  adding a
  $ hg branch
  default
  $ hg branch test
  marked working directory as branch test
  (branches are permanent and global, did you want a bookmark?)
  $ echo "bbb" >> a
  $ hg status
  M a
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch
  default
  $ echo "bbb" >> b
  $ hg status
  ? b
  $ hg unshelve
  unshelving change 'default'
  marked working directory as branch test
  $ hg status
  M a
  ? b
  $ hg branch
  test
  $ cd ..

When i shelve commit on newly created branch, make
some changes, unshelve it and running into merge
conflicts i expect that after fixing them and
running unshelve --continue newly created branch
will be preserved.
  $ hg init obs-shelve_on_new_branch_conflict
  $ cd obs-shelve_on_new_branch_conflict
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo "aaa" >> a
  $ hg commit -A -m "a"
  adding a
  $ hg branch
  default
  $ hg branch test
  marked working directory as branch test
  (branches are permanent and global, did you want a bookmark?)
  $ echo "bbb" >> a
  $ hg status
  M a
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch
  default
  $ echo "ccc" >> a
  $ hg status
  M a
  $ hg unshelve
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 1:425c97ef07f3 "changes to: a"
  merging a
  warning: conflicts while merging a! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ echo "aaabbbccc" > a
  $ rm a.orig
  $ hg resolve --mark a
  (no more unresolved files)
  continue: hg unshelve --continue
  $ hg unshelve --continue
  rebasing 1:425c97ef07f3 "changes to: a"
  marked working directory as branch test
  unshelve of 'default' complete
  $ cat a
  aaabbbccc
  $ hg status
  M a
  $ hg branch
  test
  $ hg commit -m "test-commit"

When i shelve on test branch, update to default branch
and unshelve i expect that it will not preserve previous
test branch.
  $ echo "xxx" > b
  $ hg add b
  $ hg shelve
  shelved as test
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg update -r default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg unshelve
  unshelving change 'test'
  rebasing shelved changes
  rebasing *:357525f34729 "changes to: test-commit"* (glob)
  $ hg status
  A b
  $ hg branch
  default
  $ cd ..

When i unshelve resulting in merge conflicts and makes saved
file shelvedstate looks like in previous versions in
mercurial(without restore branch information in 7th line) i
expect that after resolving conflicts and successfully
running 'shelve --continue' the branch information won't be
restored and branch will be unchanged.

shelve on new branch, conflict with previous shelvedstate
  $ hg init obs-conflict
  $ cd obs-conflict
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo "aaa" >> a
  $ hg commit -A -m "a"
  adding a
  $ hg branch
  default
  $ hg branch test
  marked working directory as branch test
  (branches are permanent and global, did you want a bookmark?)
  $ echo "bbb" >> a
  $ hg status
  M a
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch
  default
  $ echo "ccc" >> a
  $ hg status
  M a
  $ hg unshelve
  unshelving change 'default'
  temporarily committing pending changes (restore with 'hg unshelve --abort')
  rebasing shelved changes
  rebasing 1:425c97ef07f3 "changes to: a"
  merging a
  warning: conflicts while merging a! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]

Removing restore branch information from shelvedstate file(making it looks like
in previous versions) and running unshelve --continue
  $ cp .hg/shelvedstate .hg/shelvedstate_old
  $ cat .hg/shelvedstate_old | grep -v 'branchtorestore' > .hg/shelvedstate
  $ echo "aaabbbccc" > a
  $ rm a.orig
  $ hg resolve --mark a
  (no more unresolved files)
  continue: hg unshelve --continue
  $ hg unshelve --continue
  rebasing 1:425c97ef07f3 "changes to: a"
  unshelve of 'default' complete
  $ cat a
  aaabbbccc
  $ hg status
  M a
  $ hg branch
  default
  $ cd ..

On non bare shelve the branch information shouldn't be restored
  $ hg init obssh-bare_shelve_on_new_branch
  $ cd obssh-bare_shelve_on_new_branch
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo "aaa" >> a
  $ hg commit -A -m "a"
  adding a
  $ hg branch
  default
  $ hg branch test
  marked working directory as branch test
  (branches are permanent and global, did you want a bookmark?)
  $ echo "bbb" >> a
  $ hg status
  M a
  $ hg shelve a
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch
  test
  $ hg branch default
  marked working directory as branch default
  (branches are permanent and global, did you want a bookmark?)
  $ echo "bbb" >> b
  $ hg status
  ? b
  $ hg unshelve
  unshelving change 'default'
  $ hg status
  M a
  ? b
  $ hg branch
  default
  $ cd ..

Prepare unshelve with a corrupted shelvedstate
  $ hg init obssh-r1 && cd obssh-r1
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo text1 > file && hg add file
  $ hg shelve
  shelved as default
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ echo text2 > file && hg ci -Am text1
  adding file
  $ hg unshelve
  unshelving change 'default'
  rebasing shelved changes
  rebasing 0:396ea74229f9 "(changes in empty repository)"
  merging file
  warning: conflicts while merging file! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ echo somethingsomething > .hg/shelvedstate

Unshelve --continue fails with appropriate message if shelvedstate is corrupted
  $ hg unshelve --continue
  abort: corrupted shelved state file
  (please run hg unshelve --abort to abort unshelve operation)
  [255]

Unshelve --abort works with a corrupted shelvedstate
  $ hg unshelve --abort
  could not read shelved state file, your working copy may be in an unexpected state
  please update to some commit

Unshelve --abort fails with appropriate message if there's no unshelve in
progress
  $ hg unshelve --abort
  abort: no unshelve in progress
  [255]
  $ cd ..

Unshelve respects --keep even if user intervention is needed
  $ hg init obs-unshelvekeep && cd obs-unshelvekeep
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo 1 > file && hg ci -Am 1
  adding file
  $ echo 2 >> file
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ echo 3 >> file && hg ci -Am 13
  $ hg shelve --list
  default         (*s ago)    changes to: 1 (glob)
  $ hg unshelve --keep
  unshelving change 'default'
  rebasing shelved changes
  rebasing 1:3fbe6fbb0bef "changes to: 1"
  merging file
  warning: conflicts while merging file! (edit, then use 'hg resolve --mark')
  unresolved conflicts (see 'hg resolve', then 'hg unshelve --continue')
  [1]
  $ hg resolve --mark file
  (no more unresolved files)
  continue: hg unshelve --continue
  $ hg unshelve --continue
  rebasing 1:3fbe6fbb0bef "changes to: 1"
  unshelve of 'default' complete
  $ hg shelve --list
  default         (*s ago)    changes to: 1 (glob)
  $ cd ..

Unshelving a stripped commit aborts with an explanatory message
  $ hg init obs-unshelve-stripped-commit && cd obs-unshelve-stripped-commit
  $ cat <<EOF >> .hg/hgrc
  > [experimental]
  > evolution=createmarkers
  > obsshelve=True
  > EOF
  $ echo 1 > file && hg ci -Am 1
  adding file
  $ echo 2 >> file
  $ hg shelve
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg strip -r 1 --config experimental.evolution=! --hidden
  obsolete feature not enabled but 1 markers found!
  saved backup bundle to .* (re)
  $ hg unshelve
  unshelving change 'default'
  abort: shelved node 3fbe6fbb0bef4b761af46e9a7456f02877469fa0 not found in repo
  [255]

Enabling both shelve and obsshelve should not be allowed
  $ hg --config extensions.obsshelve=$TESTDIR/../hgext3rd/obsshelve.py --config extensions.shelve= log -r .
  extension 'shelve' overrides commands: * (glob)
  abort: shelve must be disabled when obsshelve is enabled
  [255]
  $ cd ..

Obsshelve knows how to unshelve traditional shelves
  $ hg init tradshelves && cd tradshelves
  $ echo root > root && hg ci -Am root
  adding root
  $ echo something >> root
  $ hg diff
  diff --git a/root b/root
  --- a/root
  +++ b/root
  @@ -1,1 +1,2 @@
   root
  +something
  $ hg shelve --config extensions.obsshelve=! --config extensions.shelve=
  shelved as default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ ls .hg/shelved/default.hg  # .hg extension indicates a traditional shelve
  .hg/shelved/default.hg
  $ hg unshelve --keep
  unshelving change 'default'
  $ hg diff
  diff --git a/root b/root
  --- a/root
  +++ b/root
  @@ -1,1 +1,2 @@
   root
  +something
  $ cd ..

