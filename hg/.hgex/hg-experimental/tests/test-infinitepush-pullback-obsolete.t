
  $ setup() {
  > cat << EOF >> .hg/hgrc
  > [extensions]
  > fbamend=$TESTDIR/../hgext3rd/fbamend
  > [experimental]
  > evolution=createmarkers
  > EOF
  > }

  $ . "$TESTDIR/library.sh"
  $ . "$TESTDIR/library-infinitepush.sh"
  $ setupcommon

Setup server
  $ hg init repo
  $ cd repo
  $ setupserver
  $ cd ..

Setup backupsource
  $ hg clone ssh://user@dummy/repo backupsource -q
  $ cd backupsource
  $ setup

Do a normal backup
  $ mkcommit first
  $ hg pushbackup
  starting backup .* (re)
  searching for changes
  remote: pushing 1 commit:
  remote:     b75a450e74d5  first
  finished in \d+\.(\d+)? seconds (re)

Make a commit, than prune a commit, than create a bookmark on top of it.
Do a backup and try to restore. Make sure it doesn't fail
  $ hg up -q null
  $ mkcommit tobepruned
  created new head
  $ hg log -r . -T '{node}\n'
  edb281c9cc7e2e51c382b6f254d1967fdfa5e6ff
  $ hg prune .
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved
  working directory now at 000000000000
  1 changesets pruned
  $ hg --hidden book -r edb281c9cc7e2e51c382b6f254d1967fdfa5e6ff newbookonpruned
  $ hg pushbackup
  starting backup .* (re)
  nothing to backup
  finished in \d+\.(\d+)? seconds (re)

Restore the repo
  $ cd ..
  $ hg clone ssh://user@dummy/repo restored -q
  $ cd restored
  $ hg pullbackup
  pulling from ssh://user@dummy/repo
  adding changesets
  adding manifests
  adding file changes
  added 1 changesets with 1 changes to 1 files
  (run 'hg update' to get a working copy)
