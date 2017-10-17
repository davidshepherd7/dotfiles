# sampling.py - sample collection extension
#
# Copyright 2016 Facebook
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.
#
# Usage:
# - This extension enhances ui.log(category, message, key=value, ...)
# to also append filtered logged events as JSON to a file.
# - The events are separated by NULL characters: '\0'.
# - The file is either specified with the SCM_SAMPLING_FILEPATH environment
# variable or the sampling.filepath configuration.
# - If the file cannot be created or accessed, fails silently
#
# The configuration details can be found in the documentation of ui.log below
from mercurial import encoding, util

import json, os

def _parentfolderexists(f):
    return (f is not None and
            os.path.exists(os.path.dirname(os.path.normpath(f))))

def _getcandidatelocation(ui):
    for candidatelocation in (
            encoding.environ.get("SCM_SAMPLING_FILEPATH", None),
            ui.config("sampling", "filepath", "")):
        if _parentfolderexists(candidatelocation):
            return candidatelocation
    return None

def uisetup(ui):
    class logtofile(ui.__class__):
        @classmethod
        def computesamplingfilters(cls, self):
            filtermap = {}
            for k in ui.configitems("sampling"):
                if not k[0].startswith("key."):
                    continue # not a key
                filtermap[k[0].lstrip("key.")]  = k[1]
            return filtermap

        def log(self, event, *msg, **opts):
            """Redirect filtered log event to a sampling file
            The configuration looks like:
            [sampling]
            filepath = path/to/file
            key.eventname = value
            key.eventname2 = value2

            If an event name appears in the config, it is logged to the
            samplingfile augmented with value stored as ref.

            Example:
            [sampling]
            filepath = path/to/file
            key.perfstatus = perf_status

            Assuming that we call:
            ui.log('perfstatus', t=3)
            ui.log('perfcommit', t=3)
            ui.log('perfstatus', t=42)

            Then we will log in path/to/file, two JSON strings separated by \0
            one for each perfstatus, like:
            {"event":"perfstatus",
             "ref":"perf_status",
             "msg":"",
             "opts":{"t":3}}\0
            {"event":"perfstatus",
             "ref":"perf_status",
             "msg":"",
             "opts":{"t":42}}\0
            """
            if not util.safehasattr(self, 'samplingfilters'):
                self.samplingfilters = logtofile.computesamplingfilters(self)
            if event not in self.samplingfilters:
                return super(logtofile, self).log(event, *msg, **opts)

            ref = self.samplingfilters[event]
            script = _getcandidatelocation(ui)
            if script:
                try:
                    opts["metrics_type"] = event
                    if msg:
                        # ui.log treats msg as a format string + format args.
                        opts["msg"] = msg[0] % msg[1:]
                    with open(script, 'a') as outfile:
                        outfile.write(json.dumps({"data": opts,
                                                  "category": ref}))
                        outfile.write("\0")
                except EnvironmentError:
                    pass
            return super(logtofile, self).log(event, *msg, **opts)

    # Replace the class for this instance and all clones created from it:
    ui.__class__ = logtofile
