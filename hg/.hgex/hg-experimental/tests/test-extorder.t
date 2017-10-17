Set up repository

  $ hg init repo
  $ cd repo
  $ echo "[extensions]" >> .hg/hgrc
  $ echo "extorder = $TESTDIR/../hgext3rd/extorder.py" >> .hg/hgrc
  $ echo "rebase =" >> .hg/hgrc
  $ echo "dummyext1 = $TESTDIR/dummyext1.py" >> .hg/hgrc
  $ echo "dummyext2 = $TESTDIR/dummyext2.py" >> .hg/hgrc
  $ echo "histedit =" >> .hg/hgrc

Simple Dependency

  $ hg id
  ext1: uisetup
  ext2: uisetup
  ext1: extsetup
  ext2: extsetup
  000000000000 tip

  $ cat >> .hg/hgrc << EOF
  > [extorder]
  > dummyext1 = dummyext2
  > preferfirst = histedit
  > preferlast = rebase
  > EOF

  $ hg id
  ext1: uisetup
  ext2: uisetup
  ext2: extsetup
  ext1: extsetup
  000000000000 tip

Conflicting deps

  $ echo "dummyext2 = dummyext1" >> .hg/hgrc
  $ hg id > out.txt 2>&1
  [1]
  $ grep MercurialExtOrderException < out.txt
  hgext_extorder.MercurialExtOrderException: extorder: conflicting extension order
