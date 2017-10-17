# dialect.py
#
# Copyright 2016 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

"""replace terms with more widely used equivalents

With this extension enabled, some terms will be replaced by their more
well-known equivalents. Namely, "changeset" will be replaced by "commit".
"""

testedwith = 'ships-with-fb-hgext'

from mercurial import (
    extensions,
    i18n,
)

def _ugettext(orig, message):
    if orig:
        message = orig(message)
    message = message.replace('changeset', 'commit')
    return message

def uisetup(ui):
    extensions.wrapfunction(i18n, '_ugettext', _ugettext)
