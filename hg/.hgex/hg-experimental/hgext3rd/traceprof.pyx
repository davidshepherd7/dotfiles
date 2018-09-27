# distutils: language = c++

# traceprof.pyx - C++ to Python bridge for the traceprof Mercurial extension
#
# Copyright 2017 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

"""accurate callgraph profiling

lsprof's high precision, plus statprof's intuitive output format.

Config::

    [traceprof]
    # whether to disable Python GC before profiling
    disablegc = no

    # minimal microseconds to show a function
    timethreshold = 2000

    # minimal call count to show "(N times)"
    countthreshold = 2

    # frame de-duplication (slower to print outputs)
    framededup = yes
"""

from libc.stdio cimport FILE
from cpython.object cimport PyObject

import contextlib
import gc

cdef extern from "traceprofimpl.cpp":
    void enable()
    void disable()
    void report(FILE *)
    void settimethreshold(double)
    void setcountthreshold(size_t)
    void setdedup(int)
    void clear()

cdef extern from "Python.h":
    FILE* PyFile_AsFile(PyObject *p)

@contextlib.contextmanager
def profile(ui, fp):
    if ui is not None:
        if ui.configbool('traceprof', 'disablegc'):
            gc.disable() # slightly more predictable
        microseconds = ui.configint('traceprof', 'timethreshold')
        if microseconds is not None:
            settimethreshold((<double>microseconds) / 1000.0)
        count = ui.configint('traceprof', 'countthreshold')
        if count is not None:
            setcountthreshold(count)
        dedup = ui.configbool('traceprof', 'framededup', True)
        setdedup(<int>dedup)
    enable()
    try:
        yield
    finally:
        disable()
        report(PyFile_AsFile(<PyObject *>fp))
        clear()
