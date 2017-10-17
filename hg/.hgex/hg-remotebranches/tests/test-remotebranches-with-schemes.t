  $ cat >> $HGRCPATH <<EOF
  > [extensions]
  > graphlog=
  > schemes=
  > EOF
  $ echo "remotebranches=$(echo $(dirname $TESTDIR))/hg_remotebranches.py" >> $HGRCPATH

  $ cat >> $HGRCPATH <<EOF
  > 
  > [schemes]
  > dotdot = ../{1}
  > EOF

  $ FILTERPWD="sed s%$PWD/%%g"

  $ mkcommit()
  > {
  >     echo $1 > $1
  >     hg add $1
  >     hg ci -m "add $1" | grep -v 'created new head'
  > }

  $ hg init alpha
  $ cd alpha
  $ mkcommit a
  [1]
  $ mkcommit b
  [1]
  $ hg branch stable | grep -v 'permanent and global'
  marked working directory as branch stable
  $ mkcommit c
  [1]
  $ cd ..
  $ hg clone alpha beta | $FILTERPWD
  updating to branch default
  2 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ cd beta
  $ mkcommit d
  [1]
  $ hg co -C stable
  1 files updated, 0 files merged, 1 files removed, 0 files unresolved
  $ hg merge default
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ hg ci -m 'merged'
  $ cd ..

  $ hg init gamma
  $ cd gamma
  $ cat > .hg/hgrc <<EOF
  > [paths]
  > default = ../alpha
  > alpha = ../alpha
  > EOF
Mercurial 3.5 added support for include statements in repo-level hgrc
files, which managed to give us a little breakage. Insert an include
line only for 3.5 and later.
  $ cat > writerc.py <<EOF
  > from mercurial import util
  > import os
  > if util.version() > '3.5':
  >   with open('.hg/hgrc', 'a') as f:
  >     f.write('%%include %s/narf\n' % os.environ['TESTTMP'])
  >   with open('%s/narf' % os.environ['TESTTMP'], 'a') as f:
  >     f.write('[paths]\nbeta = dotdot://beta/\n')
  > else:
  >   with open('.hg/hgrc', 'a') as f:
  >     f.write('beta = dotdot://beta/\n')
  > EOF
  $ python writerc.py
  $ rm writerc.py
  $ hg pull | $FILTERPWD
  pulling from alpha
  requesting all changes
  adding changesets
  adding manifests
  adding file changes
  added 3 changesets with 3 changes to 3 files
  (run 'hg update' to get a working copy)
  $ hg pull beta | $FILTERPWD
  pulling from dotdot://beta/
  searching for changes
  adding changesets
  adding manifests
  adding file changes
  added 2 changesets with 1 changes to 1 files
  (run 'hg update' to get a working copy)
  $ hg co -C default
  3 files updated, 0 files merged, 0 files removed, 0 files unresolved
  $ hg branch default | grep -v 'permanent and global'
  marked working directory as branch default
  $ mkcommit e
  [1]
  $ hg merge stable
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ hg ci -m 'merging stable'

graph shows tags for the branch heads of each path
  $ hg log --graph
  @    changeset:   6:ce61ec32ee23
  |\   tag:         tip
  | |  parent:      5:6d6442577283
  | |  parent:      4:8948da77173b
  | |  user:        test
  | |  date:        Thu Jan 01 00:00:00 1970 +0000
  | |  summary:     merging stable
  | |
  | o  changeset:   5:6d6442577283
  | |  parent:      3:78f83396d79e
  | |  user:        test
  | |  date:        Thu Jan 01 00:00:00 1970 +0000
  | |  summary:     add e
  | |
  o |  changeset:   4:8948da77173b
  |\|  branch:      stable
  | |  tag:         beta/stable
  | |  parent:      2:95cb4ab9fe1d
  | |  parent:      3:78f83396d79e
  | |  user:        test
  | |  date:        Thu Jan 01 00:00:00 1970 +0000
  | |  summary:     merged
  | |
  | o  changeset:   3:78f83396d79e
  | |  tag:         beta/default
  | |  parent:      1:7c3bad9141dc
  | |  user:        test
  | |  date:        Thu Jan 01 00:00:00 1970 +0000
  | |  summary:     add d
  | |
  o |  changeset:   2:95cb4ab9fe1d
  |/   branch:      stable
  |    tag:         alpha/stable
  |    user:        test
  |    date:        Thu Jan 01 00:00:00 1970 +0000
  |    summary:     add c
  |
  o  changeset:   1:7c3bad9141dc
  |  tag:         alpha/default
  |  user:        test
  |  date:        Thu Jan 01 00:00:00 1970 +0000
  |  summary:     add b
  |
  o  changeset:   0:1f0dee641bb7
     user:        test
     date:        Thu Jan 01 00:00:00 1970 +0000
     summary:     add a
  
