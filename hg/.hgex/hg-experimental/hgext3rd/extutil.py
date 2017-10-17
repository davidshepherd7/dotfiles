# extutil.py - useful utility methods for extensions
#
# Copyright 2016 Facebook
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

import errno
import os
import platform
import subprocess

if platform.system() == 'Windows':
    # no fork on Windows, but we can create a detached process
    # https://msdn.microsoft.com/en-us/library/windows/desktop/ms684863.aspx
    # No stdlib constant exists for this value
    DETACHED_PROCESS = 0x00000008
    _creationflags = DETACHED_PROCESS | subprocess.CREATE_NEW_PROCESS_GROUP

    def runbgcommand(script, env, shell=False, stdout=None, stderr=None):
        '''Spawn a command without waiting for it to finish.'''
        # we can't use close_fds *and* redirect stdin. I'm not sure that we
        # need to because the detached process has no console connection.
        subprocess.Popen(
            script, shell=shell, env=env, close_fds=True,
            creationflags=_creationflags, stdout=stdout, stderr=stderr)
else:
    def runbgcommand(cmd, env, shell=False, stdout=None, stderr=None):
        '''Spawn a command without waiting for it to finish.'''
        # double-fork to completely detach from the parent process
        # based on http://code.activestate.com/recipes/278731
        pid = os.fork()
        if pid:
            # Parent process
            (_pid, status) = os.waitpid(pid, 0)
            if os.WIFEXITED(status):
                returncode = os.WEXITSTATUS(status)
            else:
                returncode = -os.WTERMSIG(status)
            if returncode != 0:
                # The child process's return code is 0 on success, an errno
                # value on failure, or 255 if we don't have a valid errno
                # value.
                #
                # (It would be slightly nicer to return the full exception info
                # over a pipe as the subprocess module does.  For now it
                # doesn't seem worth adding that complexity here, though.)
                if returncode == 255:
                    returncode = errno.EINVAL
                raise OSError(returncode, 'error running %r: %s' %
                              (cmd, os.strerror(returncode)))
            return

        returncode = 255
        try:
            # Start a new session
            os.setsid()

            stdin = open(os.devnull, 'r')
            if stdout is None:
                stdout = open(os.devnull, 'w')
            if stderr is None:
                stderr = open(os.devnull, 'w')

            # connect stdin to devnull to make sure the subprocess can't
            # muck up that stream for mercurial.
            subprocess.Popen(
                cmd, shell=shell, env=env, close_fds=True,
                stdin=stdin, stdout=stdout, stderr=stderr)
            returncode = 0
        except EnvironmentError as ex:
            returncode = (ex.errno & 0xff)
            if returncode == 0:
                # This shouldn't happen, but just in case make sure the
                # return code is never 0 here.
                returncode = 255
        except Exception:
            returncode = 255
        finally:
            # mission accomplished, this child needs to exit and not
            # continue the hg process here.
            os._exit(returncode)

def runshellcommand(script, env):
    '''
    Run a shell command in the background.
    This spawns the command and returns before it completes.

    Prefer using runbgcommand() instead of this function.  This function should
    be discouraged in new code.  Running commands through a subshell requires
    you to be very careful about correctly escaping arguments, and you need to
    make sure your command works with both Windows and Unix shells.
    '''
    runbgcommand(script, env=env, shell=True)

def replaceclass(container, classname):
    '''Replace a class with another in a module, and interpose it into
    the hierarchies of all loaded subclasses. This function is
    intended for use as a decorator.

      import mymodule
      @replaceclass(mymodule, 'myclass')
      class mysubclass(mymodule.myclass):
          def foo(self):
              f = super(mysubclass, self).foo()
              return f + ' bar'

    Existing instances of the class being replaced will not have their
    __class__ modified, so call this function before creating any
    objects of the target type.
    '''
    def wrap(cls):
        oldcls = getattr(container, classname)
        for subcls in oldcls.__subclasses__():
            if subcls is not cls:
                assert oldcls in subcls.__bases__
                newbases = [oldbase
                            for oldbase in subcls.__bases__
                            if oldbase != oldcls]
                newbases.append(cls)
                subcls.__bases__ = tuple(newbases)
        setattr(container, classname, cls)
        return cls
    return wrap
