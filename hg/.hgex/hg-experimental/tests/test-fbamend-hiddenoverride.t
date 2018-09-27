  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > blackbox=
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > drawdag=$RUNTESTDIR/drawdag.py
  > [experimental]
  > evolution = all
  > [blackbox]
  > track = command, commandfinish, commandexception,
  >   pinnednodes
  > EOF

  $ hg init
  $ hg debugdrawdag <<'EOS'
  > B C   # amend: B -> C
  > |/
  > A
  > EOS

  $ rm .hg/localtags
  $ hg log -G -T '{rev} {desc}\n'
  o  2 C
  |
  o  0 A
  
  $ hg log -G -T '{rev} {desc}\n' --hidden
  o  2 C
  |
  | x  1 B
  |/
  o  0 A
  
Changing working copy parent pins a node

  $ hg update 1 --hidden -q
  $ hg update 0 -q
  $ hg log -G -T '{rev} {desc}\n'
  o  2 C
  |
  | x  1 B
  |/
  @  0 A
  
Strip/prune unpins a node

  $ hg prune 1 -q
  advice: 'hg hide' provides a better UI for hiding commits
  $ hg log -G -T '{rev} {desc}\n'
  o  2 C
  |
  @  0 A
  
Bookmark pins nodes even after removed

  $ hg bookmark -ir 1 BOOK --hidden -q
  $ hg bookmark -d BOOK -q
  $ hg log -G -T '{rev} {desc}\n'
  o  2 C
  |
  | x  1 B
  |/
  @  0 A
  
Check blackbox logs

  $ hg blackbox -l 10000
  *> init exited 0 after * seconds (glob)
  *> debugdrawdag (glob)
  *> pinnednodes: ['debugdrawdag'] newpin=[] newunpin=['112478962961'] before=[] after=[] (glob)
  *> debugdrawdag exited 0 after * (glob)
  *> log -G -T '{rev} {desc}\n' (glob)
  *> log -G -T '{rev} {desc}\n' exited 0 after * (glob)
  *> log -G -T '{rev} {desc}\n' --hidden (glob)
  *> log -G -T '{rev} {desc}\n' --hidden exited 0 after * (glob)
  *> update 1 --hidden -q (glob)
  *> pinnednodes: ['update', '1', '--hidden', '-q'] newpin=['112478962961'] newunpin=[] before=[] after=['112478962961'] (glob)
  *> update 1 --hidden -q exited 0 after * (glob)
  *> update 0 -q (glob)
  *> update 0 -q exited 0 after * (glob)
  *> log -G -T '{rev} {desc}\n' (glob)
  *> log -G -T '{rev} {desc}\n' exited 0 after * (glob)
  *> prune 1 -q (glob)
  *> pinnednodes: ['prune', '1', '-q'] newpin=[] newunpin=['112478962961'] before=['112478962961'] after=[] (glob)
  *> prune 1 -q exited 0 after * (glob)
  *> log -G -T '{rev} {desc}\n' (glob)
  *> log -G -T '{rev} {desc}\n' exited 0 after * (glob)
  *> bookmark -ir 1 BOOK --hidden -q (glob)
  *> pinnednodes: ['bookmark', '-ir', '1', 'BOOK', '--hidden', '-q'] newpin=['112478962961'] newunpin=[] before=[] after=['112478962961'] (glob)
  *> bookmark -ir 1 BOOK --hidden -q exited 0 after * (glob)
  *> bookmark -d BOOK -q (glob)
  *> bookmark -d BOOK -q exited 0 after * (glob)
  *> log -G -T '{rev} {desc}\n' (glob)
  *> log -G -T '{rev} {desc}\n' exited 0 after * (glob)
  *> blackbox -l 10000 (glob)

The order matters - putting bookmarks or moving working copy on non-obsoleted
commits do not pin them. Test this using "debugobsolete" which will not call
"createmarkers".

Obsolete working copy, and move working copy away should make things disappear

  $ rm -rf .hg && hg init && hg debugdrawdag <<'EOS'
  > C E
  > | |
  > B D
  > |/
  > A
  > EOS

  $ hg up -q E
  $ hg debugobsolete `HGPLAIN=1 hg log -r E -T '{node}'`
  obsoleted 1 changesets
  $ hg tag --local --remove E
  $ hg log -G -T '{rev} {desc}\n'
  @  4 E
  |
  | o  3 C
  | |
  o |  2 D
  | |
  | o  1 B
  |/
  o  0 A
  
  $ hg debugobsolete `HGPLAIN=1 hg log -r D -T '{node}'`
  obsoleted 1 changesets
  $ hg tag --local --remove D
  $ hg log -G -T '{rev} {desc}\n'
  @  4 E
  |
  | o  3 C
  | |
  x |  2 D
  | |
  | o  1 B
  |/
  o  0 A
  
  $ hg update -q C
  $ hg log -G -T '{rev} {desc}\n'
  @  3 C
  |
  o  1 B
  |
  o  0 A
  
Having a bookmark on a commit, obsolete the commit, remove the bookmark

  $ rm -rf .hg && hg init && hg debugdrawdag <<'EOS'
  > C E
  > | |
  > B D
  > |/
  > A
  > EOS

  $ hg bookmark -i book-e -r E
  $ hg debugobsolete `HGPLAIN=1 hg log -r D -T '{node}'`
  obsoleted 1 changesets
  $ hg debugobsolete `HGPLAIN=1 hg log -r E -T '{node}'`
  obsoleted 1 changesets
  $ rm .hg/localtags
  $ hg log -G -T '{rev} {desc} {bookmarks}\n'
  x  4 E book-e
  |
  | o  3 C
  | |
  x |  2 D
  | |
  | o  1 B
  |/
  o  0 A
  
  $ hg bookmark -d book-e
  $ hg log -G -T '{rev} {desc} {bookmarks}\n'
  o  3 C
  |
  o  1 B
  |
  o  0 A
  
Uncommit and hiddenoverride. This is uncommon but the last uncommit should make
"A" invisible:

  $ cat >> $HGRCPATH <<EOF
  > [extensions]
  > uncommit =
  > EOF

  $ hg init $TESTTMP/repo2
  $ cd $TESTTMP/repo2
  $ hg debugdrawdag <<'EOS'
  >   B
  >   |
  >   A
  >   |
  >   Z
  > EOS

  $ eval `hg tags -T '{tag}={node}\n'`
  $ rm .hg/localtags

  $ hg up $A
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg uncommit
  $ hg log -T '{desc}' -G
  o  B
  |
  x  A
  |
  @  Z
  
  $ hg up -C $B
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg uncommit
  $ hg log -T '{desc}' -G
  @  A
  |
  o  Z
  
  $ hg up -C .
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved

  $ hg uncommit
  $ hg log -T '{desc}' -G
  @  Z
  
