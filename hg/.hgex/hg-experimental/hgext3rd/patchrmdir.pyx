# patchrmdir.py
#
# Copyright 2017 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

"""patch rmdir

Check if a directory is empty before trying to call rmdir on it. This works
around some kernel issues.
"""

from mercurial import (
    extensions,
)
import os
import errno

cdef extern from "dirent.h":
    ctypedef struct DIR
    struct dirent:
        pass

    DIR *opendir(const char *)
    dirent *readdir(DIR *)
    int closedir(DIR *)

cdef int _countdir(const char *path):
    """return min(3, the number of entries inside a directory).
    return -1 if the directory cannot be opened.
    """
    cdef DIR *d = opendir(path)
    if d == NULL:
        return -1

    cdef dirent *e
    cdef int n = 0
    while True:
        e = readdir(d)
        if e == NULL:
            break
        else:
            n += 1
            if n > 2:
                break
    closedir(d)
    return n

def _rmdir(orig, path):
    n = _countdir(path)
    if n >= 3:
        # The number 3 is because most systems have "." and "..". For systems
        # without them, we fallback to the original rmdir, the behavior should
        # still be correct.
        # Choose a slightly different error message other than "Directory not
        # empty" so the test could notice the difference.
        raise OSError(errno.ENOTEMPTY, 'Non-empty directory: %r' % path)
    else:
        return orig(path)

def uisetup(ui):
    extensions.wrapfunction(os, 'rmdir', _rmdir)
