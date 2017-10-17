
  $ . "$TESTDIR/library.sh"
  $ . "$TESTDIR/library-infinitepush.sh"
  $ setupcommon

Setup infinitepush and remotefilelog server
  $ hg init repo
  $ cd repo
  $ setupserver
  $ cat >> .hg/hgrc << EOF
  > [remotefilelog]
  > server=True
  > EOF
  $ cd ..

Make client shallow clone
  $ hgcloneshallow ssh://user@dummy/repo client
  streaming all changes
  0 files to transfer, 0 bytes of data
  transferred 0 bytes in \d+(\.\d+)? seconds \(0 bytes/sec\) (re)
  no changes found
  updating to branch default
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved

Create 3 commits, two of which will be stripped. It's important to remove file
that was created in the second commit to make sure it's filelogs won't be
downloaded to the client
  $ cd repo
  $ mkcommit serverinitialcommit
  $ mkcommit committostripfirst
  $ hg rm committostripfirst
  $ echo 'committostripsecond' >> committostripsecond
  $ hg add committostripsecond
  $ hg ci -m committostripsecond

Pull changes client-side
  $ cd ../client
  $ hg pull
  pulling from ssh://user@dummy/repo
  streaming all changes
  5 files to transfer, 1.06 KB of data
  transferred 1.06 KB in [\d.]+ seconds \([\d.]+ .*\) (re)
  searching for changes
  no changes found

Make commit on top of commit that will be stripped server-side. Also make two
bookmarks
  $ hg up 0
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  1 files fetched over 1 fetches - \(1 misses, 0.00% hit ratio\) over [\d.]+s (re)
  $ hg book goodbooktobackup
  $ hg up 2
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (leaving bookmark goodbooktobackup)
  1 files fetched over 1 fetches - \(1 misses, 0.00% hit ratio\) over [\d.]+s (re)
  $ hg book badbooktobackup
  $ mkcommit clientbadcommit
  $ hg log --graph -T '{desc} {node}'
  @  clientbadcommit 07e73d09a07862bc2b8beb13b72d2347f83e4981
  |
  o  committostripsecond 221b386ae565d9866b4838ae552ce3acc26e9fec
  |
  o  committostripfirst 48acd0edbb460dec0d93314393d41f801a9797ce
  |
  o  serverinitialcommit 22ea264ff89d6891c2889f15f338ac9fa2474f8b
  
  $ cd ..

Strip commit server-side
  $ cd repo
  $ hg log -r 1 -T '{node}\n'
  48acd0edbb460dec0d93314393d41f801a9797ce
  $ hg strip 48acd0edbb460dec0d93314393d41f801a9797ce
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  saved backup bundle to $TESTTMP/repo/.hg/strip-backup/48acd0edbb46-9d7996f9-backup.hg (glob)
  $ hg log --graph -T '{desc}'
  @  serverinitialcommit
  

 Add two revisions to a dontbackupnodes config: one is a revision that was really stripped
 from the server, another is just a node that doesn't exists in the repo
  $ cd ../client
  $ cat >> .hg/hgrc << EOF
  > [infinitepushbackup]
  > dontbackupnodes=48acd0edbb460dec0d93314393d41f801a9797ce,unknownnode
  > EOF

Now do a backup, it should not fail
  $ hg pushbackup > /dev/null
  filtering nodes: ['07e73d09a07862bc2b8beb13b72d2347f83e4981']

Now try to restore it from different client. Make sure bookmark
`goodbooktobackup` is restored
  $ cd ..
  $ hgcloneshallow ssh://user@dummy/repo secondclient
  streaming all changes
  2 files to transfer, 268 bytes of data
  transferred 268 bytes in [\d.]+ seconds \([\d.]+ .*\) (re)
  searching for changes
  no changes found
  updating to branch default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd secondclient
  $ hg pullbackup --traceback
  pulling from ssh://user@dummy/repo
  no changes found
  $ hg book
     goodbooktobackup          0:22ea264ff89d

Create a commit which deletes a file. Make sure it is backed up correctly
  $ cd ../client
  $ hg up -q 0
  $ mkcommit filetodelete
  created new head
  $ hg rm filetodelete
  $ hg ci -m 'deleted'
  $ hg log -r . -T '{node}\n'
  507709f4da22941c0471885d8377c48d6dadce21
  $ hg pushbackup > /dev/null
  filtering nodes: ['07e73d09a07862bc2b8beb13b72d2347f83e4981']
  $ scratchbookmarks
  infinitepush/backups/test/*$TESTTMP/client/bookmarks/goodbooktobackup 22ea264ff89d6891c2889f15f338ac9fa2474f8b (glob)
  infinitepush/backups/test/*$TESTTMP/client/heads/507709f4da22941c0471885d8377c48d6dadce21 507709f4da22941c0471885d8377c48d6dadce21 (glob)
