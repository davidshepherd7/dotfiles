# wirepack.py - wireprotocol for exchanging packs
#
# Copyright 2017 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from mercurial.i18n import _
from mercurial.node import nullid
import constants
import struct
from StringIO import StringIO
from collections import defaultdict

from shallowutil import readexactly, readunpack, mkstickygroupdir, readpath
import datapack, historypack, shallowutil

def sendpackpart(filename, history, data):
    """A wirepack is formatted as follows:

    wirepack = <filename len: 2 byte unsigned int><filename>
               <history len: 4 byte unsigned int>[<history rev>,...]
               <data len: 4 byte unsigned int>[<data rev>,...]

    hist rev = <node: 20 byte>
               <p1node: 20 byte>
               <p2node: 20 byte>
               <linknode: 20 byte>
               <copyfromlen: 2 byte unsigned int>
               <copyfrom>

    data rev = <node: 20 byte>
               <deltabasenode: 20 byte>
               <delta len: 8 byte unsigned int>
               <delta>
    """
    rawfilenamelen = struct.pack(constants.FILENAMESTRUCT,
                                 len(filename))
    yield '%s%s' % (rawfilenamelen, filename)

    # Serialize and send history
    historylen = struct.pack('!I', len(history))
    rawhistory = ''
    for entry in history:
        copyfrom = entry[4] or ''
        copyfromlen = len(copyfrom)
        tup = entry[:-1] + (copyfromlen,)
        rawhistory += struct.pack('!20s20s20s20sH', *tup)
        if copyfrom:
            rawhistory += copyfrom

    yield '%s%s' % (historylen, rawhistory)

    # Serialize and send data
    yield struct.pack('!I', len(data))

    # TODO: support datapack metadata
    for node, deltabase, delta in data:
        deltalen = struct.pack('!Q', len(delta))
        yield '%s%s%s%s' % (node, deltabase, deltalen, delta)

def closepart():
    return '\0' * 10

def receivepack(ui, fh, packpath):
    receiveddata = []
    receivedhistory = []
    mkstickygroupdir(ui, packpath)
    totalcount = 0
    ui.progress(_("receiving pack"), totalcount)
    with datapack.mutabledatapack(ui, packpath) as dpack:
        with historypack.mutablehistorypack(ui, packpath) as hpack:
            pendinghistory = defaultdict(dict)
            while True:
                filename = readpath(fh)
                count = 0

                # Store the history for later sorting
                for value in readhistory(fh):
                    node = value[0]
                    pendinghistory[filename][node] = value
                    receivedhistory.append((filename, node))
                    count += 1

                for node, deltabase, delta in readdeltas(fh):
                    dpack.add(filename, node, deltabase, delta)
                    receiveddata.append((filename, node))
                    count += 1

                if count == 0 and filename == '':
                    break
                totalcount += 1
                ui.progress(_("receiving pack"), totalcount)

            # Add history to pack in toposorted order
            for filename, nodevalues in sorted(pendinghistory.iteritems()):
                def _parentfunc(node):
                    p1, p2 = nodevalues[node][1:3]
                    parents = []
                    if p1 != nullid:
                        parents.append(p1)
                    if p2 != nullid:
                        parents.append(p2)
                    return parents
                sortednodes = reversed(shallowutil.sortnodes(
                                        nodevalues.iterkeys(),
                                        _parentfunc))
                for node in sortednodes:
                    node, p1, p2, linknode, copyfrom = nodevalues[node]
                    hpack.add(filename, node, p1, p2, linknode, copyfrom)
    ui.progress(_("receiving pack"), None)

    return receiveddata, receivedhistory

def readhistory(fh):
    count = readunpack(fh, '!I')[0]
    for i in xrange(count):
        entry = readunpack(fh,'!20s20s20s20sH')
        if entry[4] != 0:
            copyfrom = readexactly(fh, entry[4])
        else:
            copyfrom = ''
        entry = entry[:4] + (copyfrom,)
        yield entry

def readdeltas(fh):
    count = readunpack(fh, '!I')[0]
    for i in xrange(count):
        node, deltabase, deltalen = readunpack(fh, '!20s20sQ')
        delta = readexactly(fh, deltalen)
        yield (node, deltabase, delta)

class wirepackstore(object):
    def __init__(self, wirepack):
        self._data = {}
        self._history = {}
        fh = StringIO(wirepack)
        self._load(fh)

    def get(self, name, node):
        raise RuntimeError("must use getdeltachain with wirepackstore")

    def getdeltachain(self, name, node):
        delta, deltabase = self._data[(name, node)]
        return [(name, node, name, deltabase, delta)]

    def getmeta(self, name, node):
        try:
            size = len(self._data[(name, node)])
        except KeyError:
            raise KeyError((name, hex(node)))
        return {constants.METAKEYFLAG: '',
                constants.METAKEYSIZE: size}

    def getancestors(self, name, node, known=None):
        if known is None:
            known = set()
        if node in known:
            return []

        ancestors = {}
        seen = set()
        missing = [(name, node)]
        while missing:
            curname, curnode = missing.pop()
            info = self._history.get((name, node))
            if info is None:
                continue

            p1, p2, linknode, copyfrom = info
            if p1 != nullid and p1 not in known:
                key = (name if not copyfrom else copyfrom, p1)
                if key not in seen:
                    seen.add(key)
                    missing.append(key)
            if p2 != nullid and p2 not in known:
                key = (name, p2)
                if key not in seen:
                    seen.add(key)
                    missing.append(key)

            ancestors[curnode] = (p1, p2, linknode, copyfrom)
        if not ancestors:
            raise KeyError((name, hex(node)))
        return ancestors

    def getnodeinfo(self, name, node):
        try:
            return self._history[(name, node)]
        except KeyError:
            raise KeyError((name, hex(node)))

    def add(self, *args):
        raise RuntimeError("cannot add to a wirepack store")

    def getmissing(self, keys):
        missing = []
        for name, node in keys:
            if (name, node) not in self._data:
                missing.append((name, node))

        return missing

    def _load(self, fh):
        data = self._data
        history = self._history
        while True:
            filename = readpath(fh)
            count = 0

            # Store the history for later sorting
            for value in readhistory(fh):
                node = value[0]
                history[(filename, node)] = value[1:]
                count += 1

            for node, deltabase, delta in readdeltas(fh):
                data[(filename, node)] = (delta, deltabase)
                count += 1

            if count == 0 and filename == '':
                break

    def markledger(self, ledger, options=None):
        pass

    def cleanup(self, ledger):
        pass
