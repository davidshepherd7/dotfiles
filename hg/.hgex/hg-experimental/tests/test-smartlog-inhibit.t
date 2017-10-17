  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > inhibit=$TESTDIR/../hgext3rd/inhibit.py
  > smartlog=$TESTDIR/../hgext3rd/smartlog.py
  > [experimental]
  > evolution = createmarkers
  > EOF

Test that changesets with visible precursors are rendered as x's, even
with the inhibit extension enabled.
  $ hg init repo
  $ cd repo
  $ hg debugbuilddag +4
  $ hg book -r 3 test
  $ hg up 1
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg amend -m "amended"
  warning: the changeset's children were left behind
  (use 'hg restack' to rebase them)
  $ hg smartlog -T '{rev} {bookmarks}'
  @  4
  |
  | o  3 test
  | |
  | o  2
  | |
  | x  1 59cf2bc6d22f.preamend
  |/
  o  0
  
  $ hg unamend
  $ hg up 2
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg smartlog -T '{rev} {bookmarks}'
  o  3 test
  |
  @  2
  |
  o  1 59cf2bc6d22f.preamend
  |
  o  0
  
