  $ . $TESTDIR/require-ext.sh remotenames
  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > infinitepush=$TESTDIR/../infinitepush
  > [infinitepush]
  > branchpattern=re:scratch/.+
  > [ui]
  > ssh = python "$TESTDIR/dummyssh"
  > EOF
  $ mkcommit() {
  >    echo "$1" > "$1"
  >    hg add "$1"
  >    hg ci -m "$1"
  > }
  $ scratchnodes() {
  >    for node in `find ../repo/.hg/scratchbranches/index/nodemap/* | sort`; do
  >        echo ${node##*/}
  >    done
  > }
  $ scratchbookmarks() {
  >    for bookmark in `find ../repo/.hg/scratchbranches/index/bookmarkmap/* -type f | sort`; do
  >        echo "${bookmark##*/bookmarkmap/} `cat $bookmark`"
  >    done
  > }
  $ enableremotenames() {
  >    printf '[extensions]\nremotenames=\n' >> .hg/hgrc
  > }

Create server repo with one commit and one remote bookmark
  $ hg init repo
  $ cd repo
  $ cat >> .hg/hgrc << EOF
  > [infinitepush]
  > server=yes
  > indextype=disk
  > storetype=disk
  > EOF
  $ mkcommit servercommit
Let's make server bookmark to match scratch pattern and
check that it won't be handled like scratch bookmark
  $ hg book scratch/serverbook
  $ cd ..

Clone server and enable remotenames
  $ hg clone ssh://user@dummy/repo --config extensions.remotenames= client -q
  $ cd client
  $ enableremotenames
  $ hg book --remote
     default/scratch/serverbook 0:ac312cb08db5

Push scratch commit and scratch bookmark
  $ mkcommit scratchcommitwithremotenames
  $ hg push --config extensions.remotenames= -r . --to scratch/mybranch --create
  pushing to ssh://user@dummy/repo
  searching for changes
  remote: pushing 1 commit:
  remote:     620472ff5c0c  scratchcommitwithremotenames
  $ hg book --remote
     default/scratch/mybranch  1:620472ff5c0c
     default/scratch/serverbook 0:ac312cb08db5
  $ hg book
  no bookmarks set
  $ hg -R ../repo log -G
  @  changeset:   0:ac312cb08db5
     bookmark:    scratch/serverbook
     tag:         tip
     user:        test
     date:        Thu Jan 01 00:00:00 1970 +0000
     summary:     servercommit
  
  $ scratchnodes
  620472ff5c0c4a560a3ffd98c07f0c9ecad33f64
  $ scratchbookmarks
  scratch/mybranch 620472ff5c0c4a560a3ffd98c07f0c9ecad33f64
  $ cd ..

Clone server one more time and pull scratch bookmark. Make sure it is remote
  $ hg clone ssh://user@dummy/repo --config extensions.remotenames= client2 -q
  $ cd client2
  $ enableremotenames
  $ hg book --remote
     default/scratch/serverbook 0:ac312cb08db5
  $ hg pull -B scratch/mybranch
  pulling from ssh://user@dummy/repo
  searching for changes
  adding changesets
  adding manifests
  adding file changes
  added 1 changesets with 1 changes to 1 files
  (run 'hg update' to get a working copy)
  $ hg book --remote
     default/scratch/mybranch  1:620472ff5c0c
     default/scratch/serverbook 0:ac312cb08db5
  $ hg book
  no bookmarks set

Make sure that next non-scratch pull doesn't override remote scratch bookmarks
  $ hg pull
  pulling from ssh://user@dummy/repo
  searching for changes
  no changes found
  $ hg book --remote
     default/scratch/mybranch  1:620472ff5c0c
     default/scratch/serverbook 0:ac312cb08db5
  $ cd ..

Create one more branch head on the server
  $ cd repo
  $ mkcommit head1
  $ hg up ac312cb08db5
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  (leaving bookmark scratch/serverbook)
  $ mkcommit head2
  created new head
  $ hg log -G
  @  changeset:   2:dc4b2ecb723b
  |  tag:         tip
  |  parent:      0:ac312cb08db5
  |  user:        test
  |  date:        Thu Jan 01 00:00:00 1970 +0000
  |  summary:     head2
  |
  | o  changeset:   1:64d557aa86fd
  |/   bookmark:    scratch/serverbook
  |    user:        test
  |    date:        Thu Jan 01 00:00:00 1970 +0000
  |    summary:     head1
  |
  o  changeset:   0:ac312cb08db5
     user:        test
     date:        Thu Jan 01 00:00:00 1970 +0000
     summary:     servercommit
  
  $ cd ..

Go back to client, make pull and make sure that we pulled remote branches
  $ cd client
  $ cat .hg/remotenames | sort
  620472ff5c0c4a560a3ffd98c07f0c9ecad33f64 bookmarks default/scratch/mybranch
  ac312cb08db5366e622a01fd001e583917eb9f1c bookmarks default/scratch/serverbook
  ac312cb08db5366e622a01fd001e583917eb9f1c branches default/default
  $ hg pull
  pulling from ssh://user@dummy/repo
  searching for changes
  adding changesets
  adding manifests
  adding file changes
  added 2 changesets with 2 changes to 2 files (+2 heads)
  (run 'hg heads' to see heads, 'hg merge' to merge)
  $ cat .hg/remotenames | sort
  620472ff5c0c4a560a3ffd98c07f0c9ecad33f64 bookmarks default/scratch/mybranch
  64d557aa86fdc42384b193f7eab99059da84f1f0 bookmarks default/scratch/serverbook
  64d557aa86fdc42384b193f7eab99059da84f1f0 branches default/default
  dc4b2ecb723bbf077b2d1ecab0dace0b001eb5b8 branches default/default
  $ cd ..

Push from another client, make sure that push doesn't override scratch bookmarks
  $ cd client2
  $ hg up scratch/serverbook
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ mkcommit newscratch
  created new head
  $ hg push -r . --to scratch/secondbranch --create
  pushing to ssh://user@dummy/repo
  searching for changes
  remote: pushing 1 commit:
  remote:     36667a3f76e4  newscratch
  $ hg book --remote
     default/scratch/mybranch  1:620472ff5c0c
     default/scratch/secondbranch 2:36667a3f76e4
     default/scratch/serverbook 0:ac312cb08db5
  $ hg book
  no bookmarks set

Try to push with remotebookmarks disabled
  $ hg push --config remotenames.bookmarks=False -r . --to scratch/secondbranch
  pushing to ssh://user@dummy/repo
  searching for changes
  remote: pushing 1 commit:
  remote:     36667a3f76e4  newscratch
  $ hg book
     scratch/secondbranch      2:36667a3f76e4

Create new bookmark and try to pull it
  $ mkcommit newcommittoupdate1
  $ hg push -q -r . --to scratch/branchtoupdateto1 --create
  $ hg up -q ".^"
  $ mkcommit newcommittoupdate2
  created new head
  $ hg push -q -r . --to scratch/branchtoupdateto2 --create
  $ hg up -q ".^"
  $ mkcommit newcommittopull
  created new head
  $ hg push -q -r . --to scratch/branchtopull --create
  $ cd ../client
  $ hg up default/scratch/branchtoupdateto1
  'scratch/branchtoupdateto1' does not exist locally - looking for it remotely...
  pulling from ssh://user@dummy/repo
  searching for changes
  adding changesets
  adding manifests
  adding file changes
  added 2 changesets with 2 changes to 2 files (+1 heads)
  (run 'hg heads .' to see heads, 'hg merge' to merge)
  'scratch/branchtoupdateto1' found remotely
  2 files updated, 0 files merged, 1 files removed, 0 files unresolved

  $ cat >> $HGRCPATH << EOF
  > [remotenames]
  > hoist=remote
  > rename.default=remote
  > EOF

  $ hg up remote/scratch/branchtoupdateto2
  'scratch/branchtoupdateto2' does not exist locally - looking for it remotely...
  pulling from ssh://user@dummy/repo
  searching for changes
  adding changesets
  adding manifests
  adding file changes
  added 1 changesets with 1 changes to 2 files (+1 heads)
  (run 'hg heads .' to see heads, 'hg merge' to merge)
  'scratch/branchtoupdateto2' found remotely
  1 files updated, 0 files merged, 1 files removed, 0 files unresolved
