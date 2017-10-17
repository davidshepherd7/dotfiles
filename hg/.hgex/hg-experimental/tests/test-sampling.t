Setup. SCM_SAMPLING_FILEPATH needs to be cleared as some environments may
have it set.

  $ unset SCM_SAMPLING_FILEPATH
  $ PYTHONPATH=$TESTDIR/..:$PYTHONPATH
  $ export PYTHONPATH

  $ mkcommit() {
  >    echo "$1" > "$1"
  >    hg add "$1"
  >    echo "add $1" > msg
  >    echo "" >> msg
  >    hg ci -l msg
  > }
Init the repo
  $ hg init testrepo
  $ cd testrepo
  $ mkcommit a
Create an extension that logs every commit and also call repo.revs twice

Create an extension that logs the call to commit
  $ cat > $TESTTMP/logcommit.py << EOF
  > from mercurial import extensions, localrepo
  > def cb(sample):
  >   return len(sample)
  > def _commit(orig, repo, *args, **kwargs):
  >     repo.ui.log("commit", "match filter", k=1, a={"hi":"ho"})
  >     repo.ui.log("foo", "does not match filter", k=1, a={"hi":"ho"})
  >     repo.ui.log("commit", "message %s", "string", k=1, a={"hi":"ho"})
  >     return orig(repo, *args, **kwargs)
  > def extsetup(ui):
  >     extensions.wrapfunction(localrepo.localrepository, 'commit', _commit)
  > EOF


Set up the extension and set a log file
We whitelist only the 'commit' key, only the events with that key will be
logged
  $ cat >> $HGRCPATH << EOF
  > [sampling]
  > key.commit=commit_table
  > [extensions]
  > sampling=$TESTDIR/../hgext3rd/sampling.py
  > EOF
  $ LOGDIR=`pwd`/logs
  $ mkdir $LOGDIR
  $ echo "logcommit=$TESTTMP/logcommit.py" >> $HGRCPATH
  $ echo "[sampling]" >> $HGRCPATH
  $ echo "filepath = $LOGDIR/samplingpath.txt" >> $HGRCPATH

Do a couple of commits.  We expect to log two messages per call to repo.commit.

  $ mkcommit b
  $ mkcommit c
  >>> import json
  >>> with open("$LOGDIR/samplingpath.txt") as f:
  ...     data = f.read()
  >>> for record in data.strip("\0").split("\0"):
  ...     parsedrecord = json.loads(record)
  ...     print(' '.join([parsedrecord["data"]["msg"], parsedrecord["category"]]))
  ...     assert len(parsedrecord["data"]) == 4
  match filter commit_table
  message string commit_table
  match filter commit_table
  message string commit_table
