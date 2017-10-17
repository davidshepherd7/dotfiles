  $ . "$TESTDIR/library.sh"

  $ mkcommit() {
  >    echo "$1" > "$1"
  >    hg add "$1"
  >    hg ci -m "$1"
  > }

Create server
  $ hginit master
  $ cd master
  $ cat >> .hg/hgrc <<EOF
  > [extensions]
  > infinitepush=$TESTDIR/../infinitepush
  > [remotefilelog]
  > server=True
  > [infinitepush]
  > server=True
  > branchpattern=re:scratch/.+
  > indextype=disk
  > storetype=disk
  > EOF
  $ cd ..

Create first client
  $ hgcloneshallow ssh://user@dummy/master shallow1
  streaming all changes
  0 files to transfer, 0 bytes of data
  transferred 0 bytes in * seconds (0 bytes/sec) (glob)
  no changes found
  updating to branch default
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd shallow1
  $ cat >> .hg/hgrc <<EOF
  > [extensions]
  > infinitepush=$TESTDIR/../infinitepush
  > [infinitepush]
  > server=False
  > branchpattern=re:scratch/.+
  > EOF
  $ cd ..

Create second client
  $ hgcloneshallow ssh://user@dummy/master shallow2 -q
  $ cd shallow2
  $ cat >> .hg/hgrc <<EOF
  > [extensions]
  > infinitepush=$TESTDIR/../infinitepush
  > [infinitepush]
  > server=False
  > branchpattern=re:scratch/.+
  > EOF
  $ cd ..

First client: make commit and push to scratch branch
  $ cd shallow1
  $ mkcommit scratchcommit
  $ hg push -r . --to scratch/newscratch --create
  pushing to ssh://user@dummy/master
  searching for changes
  remote: pushing 1 commit:
  remote:     2d9cfa751213  scratchcommit
  $ cd ..

Second client: pull scratch commit and update to it
  $ cd shallow2
  $ hg pull -B scratch/newscratch
  pulling from ssh://user@dummy/master
  adding changesets
  adding manifests
  adding file changes
  added 1 changesets with 1 changes to 1 files
  (run 'hg update' to get a working copy)
  $ hg up 2d9cfa751213
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd ..

First client: make commits with file modification and file deletion
  $ cd shallow1
  $ echo 1 > 1
  $ echo 2 > 2
  $ mkdir dir
  $ echo fileindir > dir/file
  $ echo toremove > dir/toremove
  $ hg ci -Aqm 'scratch commit with many files'
  $ hg rm dir/toremove
  $ hg ci -Aqm 'scratch commit with deletion'
  $ hg push -r . --to scratch/newscratch
  pushing to ssh://user@dummy/master
  searching for changes
  remote: pushing 3 commits:
  remote:     2d9cfa751213  scratchcommit
  remote:     1c2153299e05  scratch commit with many files
  remote:     2db33e8c1f93  scratch commit with deletion
  $ cd ..

Second client: pull new scratch commits and update to all of them
  $ cd shallow2
  $ hg pull --config remotefilelog.excludepattern=somefile -B scratch/newscratch
  pulling from ssh://user@dummy/master
  searching for changes
  adding changesets
  adding manifests
  adding file changes
  added 2 changesets with 5 changes to 5 files
  (run 'hg update' to get a working copy)
  $ hg up 1c2153299e05
  4 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg up 2db33e8c1f93
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
