#require test-repo

This file is backported from mercurial/tests/test-check-shbang.t.

  $ . "$TESTDIR/helper-testrepo.sh"

look for python scripts that do not use /usr/bin/env

  $ hg files 'set:grep(r"^#!.*?python") and not grep(r"^#!/usr/bi{1}n/env python")'
  [1]

look for shell scripts that do not use /bin/sh

  $ hg files 'set:grep(r"^#!.*/bi{1}n/sh") and not grep(r"^#!/bi{1}n/sh")'
  [1]
