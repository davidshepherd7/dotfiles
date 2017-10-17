  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > tweakdefaults=$TESTDIR/../hgext3rd/tweakdefaults.py
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > rebase=
  > [experimental]
  > updatecheck=noconflict
  > EOF

Setup repo

  $ hg init repo
  $ cd repo
  $ touch a
  $ hg commit -Aqm a
  $ mkdir dir
  $ touch dir/b
  $ hg commit -Aqm b
  $ hg up -q 0
  $ echo x >> a
  $ hg commit -Aqm a2

Empty update fails

  $ hg up -q 0
  $ hg up
  abort: You must specify a destination to update to, for example "hg update master".
  (If you're trying to move a bookmark forward, try "hg rebase -d <destination>".)
  [255]

  $ hg up -q -r 1
  $ hg log -r . -T '{rev}\n'
  1
  $ hg up -q 1
  $ hg log -r . -T '{rev}\n'
  1

Updating to a specific date isn't blocked by our extensions'

  $ hg bookmark temp
  $ hg up -d "<today"
  found revision 2 from Thu Jan 01 00:00:00 1970 +0000
  1 files updated, 0 files merged, 1 files removed, 0 files unresolved
  (leaving bookmark temp)
  $ hg up temp
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (activating bookmark temp)
  $ hg bookmark --delete temp

Log is -f by default

  $ hg log -G -T '{rev} {desc}\n'
  @  1 b
  |
  o  0 a
  
  $ hg log -G -T '{rev} {desc}\n' --all
  o  2 a2
  |
  | @  1 b
  |/
  o  0 a
  
Dirty update to different rev fails with --check

  $ echo x >> a
  $ hg st
  M a
  $ hg update ".^" --check
  abort: uncommitted changes
  [255]

Dirty update allowed to same rev, with no conflicts, and --clean

  $ hg update .
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg update ".^"
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg update --clean 1
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved

Log on dir's works

  $ hg log -T '{rev} {desc}\n' dir
  1 b

  $ hg log -T '{rev} {desc}\n' -I 'dir/*'
  1 b

Empty rebase fails

  $ hg rebase
  abort: you must specify a destination (-d) for the rebase
  [255]
  $ hg rebase -d 2
  rebasing 1:7b4cb4e1674c "b"
  saved backup bundle to $TESTTMP/repo/.hg/strip-backup/7b4cb4e1674c-f22b5b1e-rebase.hg (glob)

Empty rebase with nooprebase=True (default) succeeds

  $ hg rebase -s tip -d "tip^1"
  nothing to rebase

Empty rebase with nooprebase=False fails

  $ hg rebase --config 'tweakdefaults.nooprebase=False' -s tip -d "tip^1"
  nothing to rebase
  [1]

Rebase fast forwards bookmark

  $ hg book -r 1 mybook
  $ hg up -q mybook
  $ hg log -G -T '{rev} {desc} {bookmarks}\n'
  @  1 a2 mybook
  |
  o  0 a
  
  $ hg rebase -d 2
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

  $ hg log -G -T '{rev} {desc} {bookmarks}\n'
  @  2 b mybook
  |
  o  1 a2
  |
  o  0 a
  
Rebase works with hyphens

  $ hg book -r 1 hyphen-book
  $ hg book -r 2 hyphen-dest
  $ hg up -q hyphen-book
  $ hg log --all -G -T '{rev} {desc} {bookmarks}\n'
  o  2 b hyphen-dest mybook
  |
  @  1 a2 hyphen-book
  |
  o  0 a
  
  $ hg rebase -d hyphen-dest
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

  $ hg log --all -G -T '{rev} {desc} {bookmarks}\n'
  @  2 b hyphen-book hyphen-dest mybook
  |
  o  1 a2
  |
  o  0 a
  
Grep options work

  $ mkdir -p dir1/subdir1
  $ echo str1f1 >> dir1/f1
  $ echo str1-v >> dir1/-v
  $ echo str1space >> 'dir1/file with space'
  $ echo str1sub >> dir1/subdir1/subf1
  $ hg add -q dir1

  $ hg grep x
  a:x
  $ hg grep -i X
  a:x
  $ hg grep -l x
  a
  $ hg grep -n x
  a:1:x
#if osx
  $ hg grep -V ''
#else
  $ hg grep -V ''
  [123]
#endif

Make sure grep works in subdirectories and with strange filenames
  $ cd dir1
  $ hg grep str1
  -v:str1-v
  f1:str1f1
  file with space:str1space
  subdir1/subf1:str1sub
  $ hg grep str1 'relre:f[0-9]+'
  f1:str1f1
  subdir1/subf1:str1sub

Basic vs extended regular expressions
#if osx
  $ hg grep 'str([0-9])'
  [1]
#else
  $ hg grep 'str([0-9])'
  [123]
#endif
  $ hg grep 'str\([0-9]\)'
  -v:str1-v
  f1:str1f1
  file with space:str1space
  subdir1/subf1:str1sub
#if osx
  $ hg grep -F 'str[0-9]'
  [1]
#else
  $ hg grep -F 'str[0-9]'
  [123]
#endif
  $ hg grep -E 'str([0-9])'
  -v:str1-v
  f1:str1f1
  file with space:str1space
  subdir1/subf1:str1sub

Filesets
  $ hg grep str1 'set:added()'
  -v:str1-v
  f1:str1f1
  file with space:str1space
  subdir1/subf1:str1sub

Crazy filenames
  $ hg grep str1 -- -v
  -v:str1-v
  $ hg grep str1 'glob:*v'
  -v:str1-v
  $ hg grep str1 'file with space'
  file with space:str1space
  $ hg grep str1 'glob:*with*'
  file with space:str1space
  $ hg grep str1 'glob:*f1'
  f1:str1f1
  $ hg grep str1 subdir1
  subdir1/subf1:str1sub
  $ hg grep str1 'glob:**/*f1'
  f1:str1f1
  subdir1/subf1:str1sub

Test tweaked branch command
  $ hg branch
  default
  $ hg branch foo --config tweakdefaults.allowbranch=false
  abort: new named branches are disabled in this repository
  [255]
  $ hg branch -C
  reset working directory to branch default

  $ hg branch --config tweakdefaults.allowbranch=false --config tweakdefaults.branchmessage='testing' foo
  abort: testing
  [255]
  $ hg branch --config tweakdefaults.allowbranch=false --new foo
  abort: new named branches are disabled in this repository
  [255]

  $ hg branch foo
  abort: do not use branches; use bookmarks instead
  (use --new if you are certain you want a branch)
  [255]
  $ hg branch --new foo
  marked working directory as branch foo
  (branches are permanent and global, did you want a bookmark?)
  $ hg branches --config tweakdefaults.branchesmessage='testing' > /dev/null
  testing

Test tweaked merge command
  $ hg merge | head -n1
  abort: no matching bookmark to merge - please merge with an explicit rev or bookmark
  (run 'hg heads' to see all heads)

  $ hg merge --config tweakdefaults.allowmerge=false
  abort: merging is not supported for this repository
  (use rebase instead)
  [255]

  $ hg merge --config tweakdefaults.mergemessage='testing' --config tweakdefaults.mergehint='hint' --config tweakdefaults.allowmerge=false
  abort: testing
  (hint)
  [255]

Test that status is default relative
  $ mkdir foo
  $ cd foo
  $ hg status
  A ../-v
  A ../f1
  A ../file with space
  A ../subdir1/subf1
  $ hg status --root-relative
  A dir1/-v
  A dir1/f1
  A dir1/file with space
  A dir1/subdir1/subf1
  $ hg status .
  $ hg status ../subdir1
  A ../subdir1/subf1

Test that --root-relative and patterns abort
  $ hg status --root-relative ""
  abort: --root-relative not supported with patterns
  (run from the repo root instead)
  [255]

Don't break automation
  $ HGPLAIN=1 hg status
  A dir1/-v
  A dir1/f1
  A dir1/file with space
  A dir1/subdir1/subf1

Test tweaked rollback command
  $ hg rollback --config tweakdefaults.allowrollback=false
  abort: the use of rollback is disabled
  [255]
  $ hg rollback --config tweakdefaults.allowrollback=false --config tweakdefaults.rollbackmessage='testing'
  abort: testing
  [255]
  $ hg rollback --config tweakdefaults.allowrollback=false --config tweakdefaults.rollbackmessage='testing' --config tweakdefaults.rollbackhint='hint'
  abort: testing
  (hint)
  [255]

Test tweaked tag command
  $ hg tag foo
  $ hg tag --config tweakdefaults.allowtags=false foo
  abort: new tags are disabled in this repository
  [255]
  $ hg tag --config tweakdefaults.allowtags=false --config tweakdefaults.tagmessage='testing' foo
  abort: testing
  [255]

  $ hg tags --config tweakdefaults.tagsmessage='testing' > /dev/null
  testing

Test graft date when tweakdefaults.graftkeepdate is not set
  $ hg revert -a -q
  $ hg up -q 1
  $ hg graft -q 2
  $ hg log -T "{rev}\n" -d "yesterday to today"
  4

Test graft date when tweakdefaults.graftkeepdate is not set and --date is provided
  $ hg up -q 1
  $ hg graft -q 2 --date "1 1"
  $ hg log -l 1 -T "{date} {rev}\n"
  1.01 5

Test graft date when tweakdefaults.graftkeepdate is set
  $ hg up -q 1
  $ hg graft -q 5 --config tweakdefaults.graftkeepdate=True
  $ hg log -l 1 -T "{date} {rev}\n"
  1.01 6

Test amend date when tweakdefaults.amendkeepdate is not set
  $ hg up -q 1
  $ echo x > a
  $ hg commit -Aqm "commit for amend"
  $ echo x > a
  $ hg amend -q -m "amended message"
  $ hg log -T "{rev}\n" -d "yesterday to today"
  7

Test amend date when tweakdefaults.amendkeepdate is set
  $ touch new_file
  $ hg commit -d "0 0" -Aqm "commit for amend"
  $ echo x > new_file
  $ hg commit -q --amend -m "amended message" --config tweakdefaults.amendkeepdate=True
  $ hg log -l 1 -T "{date} {rev}\n"
  0.00 8

Test commit --amend date when tweakdefaults.amendkeepdate is set
  $ echo a >> new_file
  $ hg commit -d "0 0" -Aqm "commit for amend"
  $ echo x > new_file
  $ hg commit -q --amend -m "amended message" --config tweakdefaults.amendkeepdate=True
  $ hg log -l 1 -T "{date} {rev}\n"
  0.00 9

Test commit --amend date when tweakdefaults.amendkeepdate is not set and --date is provided
  $ echo xxx > a
  $ hg commit -d "0 0" -Aqm "commit for amend"
  $ echo x > a
  $ hg commit -q --amend -m "amended message" --date "1 1"
  $ hg log -l 1 -T "{date} {rev}\n"
  1.01 10

Test rebase date when tweakdefaults.rebasekeepdate is not set
  $ echo test_1 > rebase_dest
  $ hg commit --date "1 1" -Aqm "dest commit for rebase"
  $ hg bookmark rebase_dest_test_1
  $ hg up -q ".^"
  $ echo test_1 > rebase_source
  $ hg commit --date "1 1" -Aqm "source commit for rebase"
  $ hg bookmark rebase_source_test_1
  $ hg rebase -q -s rebase_source_test_1 -d rebase_dest_test_1
  $ hg log -l 1 -T "{rev}\n" -d "yesterday to today"
  12

Test rebase date when tweakdefaults.rebasekeepdate is set
  $ echo test_2 > rebase_dest
  $ hg commit -Aqm "dest commit for rebase"
  $ hg bookmark rebase_dest_test_2
  $ hg up -q ".^"
  $ echo test_2 > rebase_source
  $ hg commit -Aqm "source commit for rebase"
  $ hg bookmark rebase_source_test_2
  $ hg rebase -q -s rebase_source_test_2 -d rebase_dest_test_2 --config tweakdefaults.rebasekeepdate=True
  $ hg log -l 2 -T "{date} {rev}\n"
  0.00 14
  0.00 13

Test reuse message flag by taking message from previous commit
  $ cd ../..
  $ hg up -q hyphen-book
  $ touch afile
  $ hg add afile
  $ hg commit -M 2
  $ hg log --template {desc} -r .
  b (no-eol)
  $ echo 'canada rocks, eh?' > afile
  $ hg commit -M . -m 'this command will fail'
  abort: --reuse-message and --message are mutually exclusive
  [255]
  $ echo 'Super duper commit message' > ../commitmessagefile
  $ hg commit -M . -l ../commitmessagefile
  abort: --reuse-message and --logfile are mutually exclusive
  [255]
  $ hg commit -M thisrevsetdoesnotexist
  abort: unknown revision 'thisrevsetdoesnotexist'!
  [255]
  $ HGEDITOR=cat hg commit -M . -e
  b
  
  
  HG: Enter commit message.  Lines beginning with 'HG:' are removed.
  HG: Leave message empty to abort commit.
  HG: --
  HG: user: test
  HG: branch 'foo'
  HG: bookmark 'hyphen-book'
  HG: changed afile

Test non-remotenames use of pull --rebase and --update requires --dest
  $ cd $TESTTMP
  $ hg clone repo clone
  updating to branch default
  9 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd clone
  $ hg pull --rebase
  abort: you must use a bookmark with tracking or manually specify a destination for the rebase
  (set up tracking with `hg book <name> -t <destination>` or manually supply --dest / -d)
  [255]
  $ hg pull --update
  abort: you must specify a destination for the update
  (use `hg pull --update --dest <destination>`)
  [255]
  $ echo foo > foo
  $ hg commit -Am 'foo'
  adding foo
  $ hg pull --rebase -d default
  pulling from $TESTTMP/repo (glob)
  searching for changes
  no changes found
  nothing to rebase - working directory parent is also destination
  $ hg pull --update -d default
  pulling from $TESTTMP/repo (glob)
  searching for changes
  no changes found
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg pull --rebase --config tweakdefaults.defaultdest=default
  pulling from $TESTTMP/repo (glob)
  searching for changes
  no changes found
  nothing to rebase - working directory parent is also destination
  $ hg pull --update --config tweakdefaults.defaultdest=default
  pulling from $TESTTMP/repo (glob)
  searching for changes
  no changes found
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd $TESTTMP

Prepare a repo for testing divergence warnings with respect to inhibit extension
and allowance of prune rebases
  $ hg init repodiv && cd repodiv
  $ cat >> .hg/hgrc << EOF
  > [experimental]
  > evolution=createmarkers
  > prunestrip=on
  > allowdivergence=off
  > [extensions]
  > strip=
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > EOF
  $ echo root > root && hg ci -Am root  # rev 0
  adding root
  $ echo a > a && hg ci -Am a  # rev 1
  adding a
  $ hg up 0 && echo b > b && hg ci -Am b  # rev 2
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  adding b
  created new head
  $ hg up 0 && echo c > c && hg ci -Am c  # rev 3
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  adding c
  created new head
  $ hg up 0 && echo d > d && hg ci -Am d  # rev 4
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  adding d
  created new head
  $ hg rebase -r 1 -d 2
  rebasing 1:09d39afb522a "a"

Test that we show divergence warning if inhibit is disabled
  $ hg rebase -r 1 -d 3 --hidden
  abort: this rebase will cause divergences from: 09d39afb522a
  (to force the rebase please set experimental.allowdivergence=True)
  [255]

Test that we do not show divergence warning if inhibit is enabled
  $ cat >> .hg/hgrc <<EOF
  > inhibit=$TESTDIR/../hgext3rd/inhibit.py
  > EOF
  $ hg rebase -r 1 -d 3 --hidden
  rebasing 1:09d39afb522a "a"

Test that we allow pure prune rebases
  $ hg strip 4
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  working directory now at 1e4be0697311
  1 changesets pruned
  $ hg rebase -r 4 -d 3 --hidden
  rebasing 4:31aefaa21905 "d"

Test diff --per-file-stat
  $ echo a >> a
  $ echo b > b
  $ hg add a b
  $ hg ci -m A
  created new head
  $ hg diff -r ".^" -r .
  diff -r 1e4be0697311 -r d17770b7624d a
  --- /dev/null	Thu Jan 01 00:00:00 1970 +0000
  +++ b/a	Thu Jan 01 00:00:00 1970 +0000
  @@ -0,0 +1,1 @@
  +a
  diff -r 1e4be0697311 -r d17770b7624d b
  --- /dev/null	Thu Jan 01 00:00:00 1970 +0000
  +++ b/b	Thu Jan 01 00:00:00 1970 +0000
  @@ -0,0 +1,1 @@
  +b
  $ hg diff -r ".^" -r . --per-file-stat-json
  {"a": {"adds": 1, "isbinary": false, "removes": 0}, "b": {"adds": 1, "isbinary": false, "removes": 0}}

Test rebase with showupdated=True
  $ cd $TESTTMP
  $ hg init showupdated
  $ cd showupdated
  $ cat >> .hg/hgrc <<EOF
  > [tweakdefaults]
  > showupdated=True
  > rebasekeepdate=True
  > EOF
  $ touch a && hg commit -Aqm a
  $ touch b && hg commit -Aqm b
  $ hg up -q 0
  $ touch c && hg commit -Aqm c
  $ hg log -G -T '{node} {rev} {bookmarks}' -r 'all()'
  @  d5e255ef74f8ec83b3a2a3f3254c699451c89e29 2
  |
  | o  0e067c57feba1a5694ca4844f05588bb1bf82342 1
  |/
  o  3903775176ed42b1458a6281db4a0ccf4d9f287a 0
  
  $ hg rebase -r 1 -d 2
  rebasing 1:0e067c57feba "b"
  0e067c57feba -> a602e0d56f83 "b"
  saved backup bundle to $TESTTMP/showupdated/.hg/strip-backup/0e067c57feba-ca6d05e3-rebase.hg (glob)

Test rebase with showupdate=True and a lot of source revisions
  $ hg up -q 0
  $ for i in `$TESTDIR/seq.py 11`; do touch "$i" && hg commit -Aqm "$i" && hg up -q 0; done
  $ hg log -G -T '{node} {rev} {bookmarks}' -r 'all()'
  o  6e3ddf6f49efd0a836a470a2d45e953db915c262 13
  |
  | o  e02ec9861284762c93b8a4c9e0bac0abfbb59ac7 12
  |/
  | o  14218977adef919e86466c688defa3fb893a5638 11
  |/
  | o  6a01a2bb0a9f7a6f01c6f49ce90e60bf85de79d0 10
  |/
  | o  e5ec40f709911f69eafada5746d3f5b969005738 9
  |/
  | o  73800d52e8ddab37a1f9177299d1fdfb563c061a 8
  |/
  | o  657f1516f142d51ba06b98413ce35a884f7f8af0 7
  |/
  | o  4e6ba707bdb81e60d96b84099c2d1c56530ee6f1 6
  |/
  | o  7ab24e484dafd1d2ebf51a4fa4523431b81b99aa 5
  |/
  | o  ee71024c6e8c4a45ee1d3e462431bfec85ac215a 4
  |/
  | o  46a418a0abd225d8ad876102f495d209907b79d9 3
  |/
  | o  a602e0d56f83e5816ebcbb78095e259ffbce94aa 2
  | |
  | o  d5e255ef74f8ec83b3a2a3f3254c699451c89e29 1
  |/
  @  3903775176ed42b1458a6281db4a0ccf4d9f287a 0
  
  $ hg rebase -r 'all() - 0 - 12' -d 12
  rebasing 1:d5e255ef74f8 "c"
  rebasing 2:a602e0d56f83 "b"
  rebasing 3:46a418a0abd2 "1"
  rebasing 4:ee71024c6e8c "2"
  rebasing 5:7ab24e484daf "3"
  rebasing 6:4e6ba707bdb8 "4"
  rebasing 7:657f1516f142 "5"
  rebasing 8:73800d52e8dd "6"
  rebasing 9:e5ec40f70991 "7"
  rebasing 10:6a01a2bb0a9f "8"
  rebasing 11:14218977adef "9"
  rebasing 13:6e3ddf6f49ef "11" (tip)
  14218977adef -> 2d12dd93bf8b "9"
  46a418a0abd2 -> 645dc4557ba8 "1"
  4e6ba707bdb8 -> b9598afdff23 "4"
  657f1516f142 -> 906a55b270d4 "5"
  6a01a2bb0a9f -> f5a7b375c4a9 "8"
  6e3ddf6f49ef -> ad74bbdbdb75 "11"
  73800d52e8dd -> 3145d47a5692 "6"
  7ab24e484daf -> 03a9e0e4badc "3"
  a602e0d56f83 -> e7a99f1fea2a "b"
  d5e255ef74f8 -> 72d850a207d5 "c"
  ...
  ee71024c6e8c -> 0c42bb4bf23f "2"
  saved backup bundle to $TESTTMP/showupdated/.hg/strip-backup/6e3ddf6f49ef-4b18babd-rebase.hg (glob)

Test rebase with showupdate=True and a long commit message
  $ touch longfile && hg add -q
  $ hg commit -qm "This is a long commit message which will be truncated."
  $ hg rebase -d 1
  rebasing 14:e915a57d67db "This is a long commit message which will be truncated." (tip)
  e915a57d67db -> 5444f740ff6c "This is a long commit message which will be tru..."
  saved backup bundle to $TESTTMP/showupdated/.hg/strip-backup/e915a57d67db-ad3372b5-rebase.hg (glob)

Test bookmark -D
  $ cd $TESTTMP
  $ hg init book-D
  $ cd book-D
  $ cat >> .hg/hgrc <<EOF
  > [extensions]
  > strip=
  > [experimental]
  > evolution=all
  > EOF
  $ hg debugbuilddag '+4*2*2*2'
  $ hg bookmark -i -r 1 master
  $ hg bookmark -i -r 5 feature1
  $ hg bookmark -i -r 6 feature2
  $ hg log -G -T '{rev} {bookmarks}' -r 'all()'
  o  6 feature2
  |
  | o  5 feature1
  | |
  o |  4
  | |
  | o  3
  |/
  o  2
  |
  o  1 master
  |
  o  0
  
  $ hg bookmark -D feature1
  bookmark 'feature1' deleted
  2 changesets pruned
  $ hg log -G -T '{rev} {bookmarks}' -r 'all()' --hidden
  o  6 feature2
  |
  | x  5
  | |
  o |  4
  | |
  | x  3
  |/
  o  2
  |
  o  1 master
  |
  o  0
  
  $ hg bookmark -D feature2 --config extensions.fbamend=!
  saved backup bundle to $TESTTMP/book-D/.hg/strip-backup/1c4dfc7a8985-4027cf08-backup.hg (glob)
  bookmark 'feature2' deleted
  $ hg log -G -T '{rev} {bookmarks}' -r 'all()' --hidden
  x  4
  |
  x  3
  |
  o  2
  |
  o  1 master
  |
  o  0
  
Test that developer warning is shown whenever ':' is used implicitly or explicitly

  $ hg log -G -T '{rev} {bookmarks}' -r '0:2'
  devel-warn: use of ':' is deprecated
   at: *tweakdefaults.py:* (_analyzewrap) (glob)
  o  2
  |
  o  1 master
  |
  o  0
  
  $ hg log -G -T '{rev} {bookmarks}' -r ':2'
  devel-warn: use of ':' is deprecated
   at: *tweakdefaults.py:* (_analyzewrap) (glob)
  o  2
  |
  o  1 master
  |
  o  0
  
  $ hg log -G -T '{rev} {bookmarks}' -r '0:'
  devel-warn: use of ':' is deprecated
   at: *tweakdefaults.py:* (_analyzewrap) (glob)
  o  2
  |
  o  1 master
  |
  o  0
  
In this testcase warning should not be shown
  $ hg log -G -T '{rev} {bookmarks}' -r ':'
  o  2
  |
  o  1 master
  |
  o  0
  
