Test whereami

  $ hg init repo1
  $ cd repo1
  $ cat > .hg/hgrc <<EOF
  > [extensions]
  > whereami=$TESTDIR/../hgext3rd/whereami.py
  > EOF

  $ hg whereami
  0000000000000000000000000000000000000000

  $ echo a > a
  $ hg add a
  $ hg commit -m a

  $ hg whereami
  cb9a9f314b8b07ba71012fcdbc544b5a4d82ff5b

  $ echo b > b
  $ hg add  b
  $ hg commit -m b

  $ hg up ".^"
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved

  $ echo c > c
  $ hg add  c
  $ hg commit -m c
  created new head

  $ hg merge 1
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)

  $ hg whereami
  d36c0562f908c692f5204d606d4ff3537d41f1bf
  d2ae7f538514cd87c17547b0de4cea71fe1af9fb
