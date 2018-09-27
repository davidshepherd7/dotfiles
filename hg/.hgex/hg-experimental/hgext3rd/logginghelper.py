# reporootlog.py - log the repo root
#
# Copyright 2016 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

"""this extension logs different pieces of information that will be used
by SCM wrappers

::

    [logging]
    # list of config options to log
    configoptions = section1.option1,section2.option2

"""

import os
from mercurial import (
    extensions,
    localrepo,
    registrar,
)

configtable = {}
configitem = registrar.configitem(configtable)

configitem('logging', 'configoptions', default=[])

def _localrepoinit(orig, self, baseui, path=None, create=False):
    orig(self, baseui, path, create)
    reponame = self.ui.config('paths', 'default')
    if reponame:
        reponame = os.path.basename(reponame)
    configoptstolog = self.ui.configlist('logging', 'configoptions')
    kwargs = {'repo': reponame}

    # The configs being read here are user defined, so we need to suppress
    # warnings telling us to register them.
    with self.ui.configoverride({("devel", "all-warnings"): False}):
        for option in configoptstolog:
            splitted = option.split('.')
            if len(splitted) != 2:
                continue
            section, name = splitted
            value = self.ui.config(section, name)
            if value is not None:
                kwargs[name] = value

    obsstore_size = 0
    try:
        obsstore_size = self.svfs.stat('obsstore').st_size
    except Exception:
        # just ignore exception, it's better than failing the whole command
        pass

    kwargs['obsstore_size'] = obsstore_size

    self.ui.log("logginghelper",
                "",           # ui.log requires a format string as args[0].
                **kwargs)

def uisetup(ui):
    extensions.wrapfunction(localrepo.localrepository,
                            '__init__', _localrepoinit)
