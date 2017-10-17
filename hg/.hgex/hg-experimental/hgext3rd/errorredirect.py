# errorredirect.py
#
# Copyright 2015 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.
"""redirect error message

Redirect error message, the stack trace, of an uncaught exception to
a custom shell script. This is useful for further handling the error,
for example posting it to a support group and logging it somewhere.

The config option errorredirect.script is the shell script to execute.
If it's empty, the extension will do nothing and fallback to the old
behavior.

Two environment variables are set: TRACE is the stack trace, which
is the same as piped content. WARNING is the warning message, which
usually contains contact message and software versions, etc.

Examples::

  [errorredirect]
  script = tee hgerr.log && echo 'Error written to hgerr.log'

  [errorredirect]
  script = echo "$WARNING$TRACE" >&2

  [errorredirect]
  script = (echo "$WARNING"; cat) | cat >&2
"""

import signal
import subprocess
import traceback
import sys

from mercurial import (
    dispatch,
    encoding,
    extensions,
)

def _printtrace(ui, warning):
    # Like dispatch.handlecommandexception, but avoids an unnecessary ui.log
    ui.warn(warning)
    return False # return value for "handlecommandexception", re-raises

def _handlecommandexception(orig, ui):
    warning = dispatch._exceptionwarning(ui)
    trace = traceback.format_exc()

    # let blackbox log it (if it is configured to do so)
    ui.log("commandexception", "%s\n%s\n", warning, trace)
    ui.log("hgerrors", "exception has occurred: %s",
           warning, type=str(sys.exc_type.__name__), traceback=trace)

    script = ui.config('errorredirect', 'script')
    if not script:
        return _printtrace(ui, warning)

    # run the external script
    env = encoding.environ.copy()
    env['WARNING'] = warning
    env['TRACE'] = trace

    # decide whether to use shell smartly, see 9335dc6b2a9c in hg
    shell = any(c in script for c in "|&;<>()$`\\\"' \t\n*?[#~=%")

    try:
        p = subprocess.Popen(script, shell=shell, stdin=subprocess.PIPE,
                             env=env)
        p.communicate(trace)
    except StandardError:
        # The binary cannot be executed, or some other issues. For example,
        # "script" is not in PATH, and shell is False; or the peer closes the
        # pipe early. Fallback to the plain error reporting.
        return _printtrace(ui, warning)
    else:
        ret = p.returncode

        # Python returns negative exit code for signal-terminated process. The
        # shell converts singal-terminated process to a positive exit code by
        # +128. Ctrl+C generates SIGTERM. Re-report the error unless the
        # process exits cleanly or is terminated by SIGTERM (Ctrl+C).
        ctrlc = (ret == signal.SIGTERM + 128) or (ret == -signal.SIGTERM)
        if ret != 0 and not ctrlc:
            return _printtrace(ui, warning)

    return True # do not re-raise

def uisetup(ui):
    extensions.wrapfunction(dispatch, 'handlecommandexception',
                            _handlecommandexception)
