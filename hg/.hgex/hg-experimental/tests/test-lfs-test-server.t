Require lfs-test-server (https://github.com/git-lfs/lfs-test-server)

  $ hash lfs-test-server || { echo 'skipped: missing lfs-test-server'; exit 80; }

  $ LFS_LISTEN="tcp://:$HGPORT"
  $ LFS_HOST="localhost:$HGPORT"
  $ LFS_PUBLIC=1
  $ export LFS_LISTEN LFS_HOST LFS_PUBLIC
  $ lfs-test-server &> lfs-server.log &
  $ echo $! >> $DAEMON_PIDS

  $ cat >> $HGRCPATH <<EOF
  > [extensions]
  > lfs=$TESTDIR/../hgext3rd/lfs
  > [lfs]
  > url=http://foo:bar@$LFS_HOST/
  > threshold=1
  > EOF

  $ hg init repo1
  $ cd repo1
  $ echo THIS-IS-LFS > a
  $ hg commit -m a -A a

  $ hg init ../repo2
  $ hg push ../repo2 -v
  pushing to ../repo2
  searching for changes
  lfs: uploading 31cf46fbc4ecd458a0943c5b4881f1f5a6dd36c53d6167d5b69ac45149b38e5b (12 bytes)
  1 changesets found
  uncompressed size of bundle content:
       * (changelog) (glob)
       * (manifests) (glob)
       *  a (glob)
  adding changesets
  adding manifests
  adding file changes
  added 1 changesets with 1 changes to 1 files

  $ cd ../repo2
  $ hg update tip -v
  resolving manifests
  getting a
  lfs: downloading 31cf46fbc4ecd458a0943c5b4881f1f5a6dd36c53d6167d5b69ac45149b38e5b (12 bytes)
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved

When the server has some blobs already

  $ hg mv a b
  $ echo ANOTHER-LARGE-FILE > c
  $ echo ANOTHER-LARGE-FILE2 > d
  $ hg commit -m b-and-c -A b c d
  $ hg push ../repo1 -v | grep -v '^  '
  pushing to ../repo1
  searching for changes
  lfs: need to transfer 2 objects (39 bytes)
  lfs: uploading 37a65ab78d5ecda767e8622c248b5dbff1e68b1678ab0e730d5eb8601ec8ad19 (20 bytes)
  lfs: uploading d11e1a642b60813aee592094109b406089b8dff4cb157157f753418ec7857998 (19 bytes)
  1 changesets found
  uncompressed size of bundle content:
  adding changesets
  adding manifests
  adding file changes
  added 1 changesets with 3 changes to 3 files

  $ hg --repo ../repo1 update tip -v
  resolving manifests
  getting b
  getting c
  lfs: downloading d11e1a642b60813aee592094109b406089b8dff4cb157157f753418ec7857998 (19 bytes)
  getting d
  lfs: downloading 37a65ab78d5ecda767e8622c248b5dbff1e68b1678ab0e730d5eb8601ec8ad19 (20 bytes)
  3 files updated, 0 files merged, 0 files removed, 0 files unresolved

Check error message when the remote missed a blob:

  $ echo FFFFF > b
  $ hg commit -m b -A b
  $ echo FFFFF >> b
  $ hg commit -m b b
  $ rm -rf .hg/store/lfs
  $ hg update -C '.^'
  abort: LFS server claims required objects do not exist:
  8e6ea5f6c066b44a0efa43bcce86aea73f17e6e23f0663df0251e7524e140a13!
  [255]
