  $ cat >> $HGRCPATH << EOF
  > [extensions]
  > show=$TESTDIR/../hgext3rd/fbshow.py
  > EOF

We assume that log basically works (it has its own tests). This just covers uses
of show that might break even if log works.

Show on empty repository: checking consistency

  $ hg init empty
  $ cd empty
  $ hg show
  changeset:   -1:000000000000
  tag:         tip
  user:        
  date:        Thu Jan 01 00:00:00 1970 +0000
  
  

  $ hg show 1
  abort: unknown revision '1'!
  [255]
  $ hg show 'branch(name)'
  abort: unknown revision 'name'!
  [255]
  $ hg show null -q
  changeset:   -1:000000000000
  tag:         tip
  user:        
  date:        Thu Jan 01 00:00:00 1970 +0000
  
  
Check various git-like options:

  $ hg init gitlike
  $ echo one > one
  $ echo two > two
  $ hg commit -qAm twofiles
  $ hg show --template status
  changeset:   0:bf7b98b60f6f
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  description:
  twofiles
  
  files:
  A one
  A two
  
  diff -r 000000000000 -r bf7b98b60f6f one
  --- /dev/null	Thu Jan 01 00:00:00 1970 +0000
  +++ b/one	Thu Jan 01 00:00:00 1970 +0000
  @@ -0,0 +1,1 @@
  +one
  diff -r 000000000000 -r bf7b98b60f6f two
  --- /dev/null	Thu Jan 01 00:00:00 1970 +0000
  +++ b/two	Thu Jan 01 00:00:00 1970 +0000
  @@ -0,0 +1,1 @@
  +two
  

Check that the command parser always treats the first argument as a revision:

  $ hg show two
  abort: unknown revision 'two'!
  [255]
  $ hg show . two
  changeset:   0:bf7b98b60f6f
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       one two
  description:
  twofiles
  
  
  diff -r 000000000000 -r bf7b98b60f6f two
  --- /dev/null	Thu Jan 01 00:00:00 1970 +0000
  +++ b/two	Thu Jan 01 00:00:00 1970 +0000
  @@ -0,0 +1,1 @@
  +two
  

Check --stat

  $ hg init stat
  $ cd stat
  $ echo show > x
  $ hg commit -qAm x
  $ hg show --stat
  changeset:   0:852a8d467a01
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       x
  description:
  x
  
  
   x |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)
  




  $ echo more >> x
  $ hg commit -qAm longer
  $ hg show --stat
  changeset:   1:b73358b94785
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       x
  description:
  longer
  
  
   x |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)
  




  $ echo remove > x
  $ hg commit -qAm remove
  $ hg show --stat
  changeset:   2:3d74ea61c11c
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       x
  description:
  remove
  
  
   x |  3 +--
   1 files changed, 1 insertions(+), 2 deletions(-)
  



  $ hg show --stat 0
  changeset:   0:852a8d467a01
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       x
  description:
  x
  
  
   x |  1 +
   1 files changed, 1 insertions(+), 0 deletions(-)
  
Check --unified and -U

  $ hg init diff
  $ cd diff
  $ cat >file <<EOF
  > line1
  > line2
  > line3
  > line4
  > line5
  > EOF
  $ hg commit -qAm file
  $ cat >>file <<EOF
  > line6
  > line7
  > line8
  > line9
  > line10
  > EOF
  $ hg commit -qm file
  $ hg show --unified=1
  changeset:   1:8e33115c1596
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       file
  description:
  file
  
  
  diff -r fd78c1ae39e0 -r 8e33115c1596 file
  --- a/file	Thu Jan 01 00:00:00 1970 +0000
  +++ b/file	Thu Jan 01 00:00:00 1970 +0000
  @@ -5,1 +5,6 @@
   line5
  +line6
  +line7
  +line8
  +line9
  +line10
  
  $ hg show --unified=2
  changeset:   1:8e33115c1596
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       file
  description:
  file
  
  
  diff -r fd78c1ae39e0 -r 8e33115c1596 file
  --- a/file	Thu Jan 01 00:00:00 1970 +0000
  +++ b/file	Thu Jan 01 00:00:00 1970 +0000
  @@ -4,2 +4,7 @@
   line4
   line5
  +line6
  +line7
  +line8
  +line9
  +line10
  

Check behavior with nonsensical integers.
  $ hg show --unified=-7
  changeset:   1:8e33115c1596
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       file
  description:
  file
  
  
  diff -r fd78c1ae39e0 -r 8e33115c1596 file
  --- a/file	Thu Jan 01 00:00:00 1970 +0000
  +++ b/file	Thu Jan 01 00:00:00 1970 +0000
  @@ -13,-14 +13,-9 @@
  +line6
  +line7
  +line8
  +line9
  +line10
  



Check --git and -g

  $ hg init git
  $ cd git
  $ echo git > file
  $ hg commit -qAm file
  $ hg show --git
  changeset:   0:2a575d662478
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       file
  description:
  file
  
  
  diff --git a/file b/file
  new file mode 100644
  --- /dev/null
  +++ b/file
  @@ -0,0 +1,1 @@
  +git
  


  $ echo more >> file
  $ hg commit -qAm file
  $ hg show -g
  changeset:   1:a23f7b259024
  tag:         tip
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       file
  description:
  file
  
  
  diff --git a/file b/file
  --- a/file
  +++ b/file
  @@ -1,1 +1,2 @@
   git
  +more
  


  $ hg show -g 0
  changeset:   0:2a575d662478
  user:        test
  date:        Thu Jan 01 00:00:00 1970 +0000
  files:       file
  description:
  file
  
  
  diff --git a/file b/file
  new file mode 100644
  --- /dev/null
  +++ b/file
  @@ -0,0 +1,1 @@
  +git
  


Check hg show '' fails to parse the revision

  $ hg show ''
  hg: parse error: empty query
  [255]

Confirm that --help works (it didn't when we used an alias)

  $ hg show --help
  hg show [OPTION]... [REV [FILE]...]
  
  show revision in detail
  
      This behaves similarly to 'hg log -vp -r REV [OPTION]... [FILE]...', or if
      called without a REV, 'hg log -vp -r . [OPTION]...' Use 'hg log' for more
      powerful operations than supported by hg show
  
      See 'hg help templates' for more about pre-packaged styles and specifying
      custom templates.
  
  (use 'hg help -e show' to show help for the show extension)
  
  options ([+] can be repeated):
  
   -g --git                 use git extended diff format
      --stat                output diffstat-style summary of changes
   -T --template TEMPLATE   display with template
   -I --include PATTERN [+] include names matching the given patterns
   -X --exclude PATTERN [+] exclude names matching the given patterns
   -U --unified VALUE       number of lines of diff context to show (default:
                            <type 'int'>)
  
  (some details hidden, use --verbose to show complete help)
  $ hg show --help --verbose
  hg show [OPTION]... [REV [FILE]...]
  
  show revision in detail
  
      This behaves similarly to 'hg log -vp -r REV [OPTION]... [FILE]...', or if
      called without a REV, 'hg log -vp -r . [OPTION]...' Use 'hg log' for more
      powerful operations than supported by hg show
  
      See 'hg help templates' for more about pre-packaged styles and specifying
      custom templates.
  
  (use 'hg help -e show' to show help for the show extension)
  
  options ([+] can be repeated):
  
   -g --git                 use git extended diff format
      --stat                output diffstat-style summary of changes
      --style STYLE         display using template map file (DEPRECATED)
   -T --template TEMPLATE   display with template
   -I --include PATTERN [+] include names matching the given patterns
   -X --exclude PATTERN [+] exclude names matching the given patterns
   -U --unified VALUE       number of lines of diff context to show (default:
                            <type 'int'>)
  
  global options ([+] can be repeated):
  
   -R --repository REPO   repository root directory or name of overlay bundle
                          file
      --cwd DIR           change working directory
   -y --noninteractive    do not prompt, automatically pick the first choice for
                          all prompts
   -q --quiet             suppress output
   -v --verbose           enable additional output
      --color TYPE        when to colorize * (glob)
                          * (glob)
      --config CONFIG [+] set/override config option (use 'section.name=value')
      --debug             enable debugging output
      --debugger          start debugger
      --encoding ENCODE   set the charset encoding (default: ascii)
      --encodingmode MODE set the charset encoding mode (default: strict)
      --traceback         always print a traceback on exception
      --time              time how long the command takes
      --profile           print command execution profile
      --version           output version information and exit
   -h --help              display help and exit
      --hidden            consider hidden changesets
      --pager TYPE        when to paginate (boolean, always, auto, or never)
                          (default: auto)
