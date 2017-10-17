#!/usr/bin/env python
import hashlib
import os
import random
import shutil
import stat
import struct
import sys
import tempfile
import time
import unittest

import silenttestrunner

# Load the local remotefilelog, not the system one
sys.path[0:0] = [os.path.join(os.path.dirname(__file__), '..')]

from remotefilelog.datapack import (
    datapack,
    fastdatapack,
    mutabledatapack,
)
from remotefilelog.basepack import (
    SMALLFANOUTCUTOFF,
    SMALLFANOUTPREFIX,
    LARGEFANOUTPREFIX,
)
from remotefilelog import constants

from mercurial.node import nullid
import mercurial.ui

class datapacktestsbase(object):
    def __init__(self, datapackreader, paramsavailable):
        self.datapackreader = datapackreader
        self.paramsavailable = paramsavailable

    def setUp(self):
        self.tempdirs = []

    def tearDown(self):
        for d in self.tempdirs:
            shutil.rmtree(d)

    def makeTempDir(self):
        tempdir = tempfile.mkdtemp()
        self.tempdirs.append(tempdir)
        return tempdir

    def getHash(self, content):
        return hashlib.sha1(content).digest()

    def getFakeHash(self):
        return ''.join(chr(random.randint(0, 255)) for _ in range(20))

    def createPack(self, revisions=None, version=0):
        if revisions is None:
            revisions = [("filename", self.getFakeHash(), nullid, "content")]

        packdir = self.makeTempDir()
        packer = mutabledatapack(mercurial.ui.ui(), packdir, version=version)

        for args in revisions:
            filename, node, base, content = args[0:4]
            # meta is optional
            meta = None
            if len(args) > 4:
                meta = args[4]
            packer.add(filename, node, base, content, metadata=meta)

        path = packer.close()
        return self.datapackreader(path)

    def _testAddSingle(self, content):
        """Test putting a simple blob into a pack and reading it out.
        """
        filename = "foo"
        node = self.getHash(content)

        revisions = [(filename, node, nullid, content)]
        pack = self.createPack(revisions)
        if self.paramsavailable:
            self.assertEquals(pack.params.fanoutprefix, SMALLFANOUTPREFIX)

        chain = pack.getdeltachain(filename, node)
        self.assertEquals(content, chain[0][4])

    def testAddSingle(self):
        self._testAddSingle('')

    def testAddSingleEmpty(self):
        self._testAddSingle('abcdef')

    def testAddMultiple(self):
        """Test putting multiple unrelated blobs into a pack and reading them
        out.
        """
        revisions = []
        for i in range(10):
            filename = "foo%s" % i
            content = "abcdef%s" % i
            node = self.getHash(content)
            revisions.append((filename, node, nullid, content))

        pack = self.createPack(revisions)

        for filename, node, base, content in revisions:
            chain = pack.getdeltachain(filename, node)
            self.assertEquals(content, chain[0][4])

    def testAddDeltas(self):
        """Test putting multiple delta blobs into a pack and read the chain.
        """
        revisions = []
        filename = "foo"
        lastnode = nullid
        for i in range(10):
            content = "abcdef%s" % i
            node = self.getHash(content)
            revisions.append((filename, node, lastnode, content))
            lastnode = node

        pack = self.createPack(revisions)
        # Test that the chain for the final entry has all the others
        chain = pack.getdeltachain(filename, node)
        for i in range(10):
            content = "abcdef%s" % i
            self.assertEquals(content, chain[-i - 1][4])

    def testPackMany(self):
        """Pack many related and unrelated objects.
        """
        # Build a random pack file
        revisions = []
        blobs = {}
        random.seed(0)
        for i in range(100):
            filename = "filename-%s" % i
            filerevs = []
            for j in range(random.randint(1, 100)):
                content = "content-%s" % j
                node = self.getHash(content)
                lastnode = nullid
                if len(filerevs) > 0:
                    lastnode = filerevs[random.randint(0, len(filerevs) - 1)]
                filerevs.append(node)
                blobs[(filename, node, lastnode)] = content
                revisions.append((filename, node, lastnode, content))

        pack = self.createPack(revisions)

        # Verify the pack contents
        for (filename, node, lastnode), content in sorted(blobs.iteritems()):
            chain = pack.getdeltachain(filename, node)
            for entry in chain:
                expectedcontent = blobs[(entry[0], entry[1], entry[3])]
                self.assertEquals(entry[4], expectedcontent)

    def testPackMetadata(self):
        revisions = []
        for i in range(100):
            filename = '%s.txt' % i
            content = 'put-something-here \n' * i
            node = self.getHash(content)
            meta = {constants.METAKEYFLAG: i ** 4,
                    constants.METAKEYSIZE: len(content),
                    'Z': 'random_string',
                    '_': '\0' * i}
            revisions.append((filename, node, nullid, content, meta))
        pack = self.createPack(revisions, version=1)
        for name, node, x, content, origmeta in revisions:
            parsedmeta = pack.getmeta(name, node)
            # flag == 0 should be optimized out
            if origmeta[constants.METAKEYFLAG] == 0:
                del origmeta[constants.METAKEYFLAG]
            self.assertEquals(parsedmeta, origmeta)

    def testPackMetadataThrows(self):
        filename = '1'
        content = '2'
        node = self.getHash(content)
        meta = {constants.METAKEYFLAG: 3}
        revisions = [(filename, node, nullid, content, meta)]
        try:
            self.createPack(revisions, version=0)
            self.assertTrue(False, "should throw if metadata is not supported")
        except RuntimeError:
            pass

    def testGetMissing(self):
        """Test the getmissing() api.
        """
        revisions = []
        filename = "foo"
        lastnode = nullid
        for i in range(10):
            content = "abcdef%s" % i
            node = self.getHash(content)
            revisions.append((filename, node, lastnode, content))
            lastnode = node

        pack = self.createPack(revisions)

        missing = pack.getmissing([("foo", revisions[0][1])])
        self.assertFalse(missing)

        missing = pack.getmissing([("foo", revisions[0][1]),
                                   ("foo", revisions[1][1])])
        self.assertFalse(missing)

        fakenode = self.getFakeHash()
        missing = pack.getmissing([("foo", revisions[0][1]), ("foo", fakenode)])
        self.assertEquals(missing, [("foo", fakenode)])

    def testAddThrows(self):
        pack = self.createPack()

        try:
            pack.add('filename', nullid, 'contents')
            self.assertTrue(False, "datapack.add should throw")
        except RuntimeError:
            pass

    def testBadVersionThrows(self):
        pack = self.createPack()
        path = pack.path + '.datapack'
        with open(path) as f:
            raw = f.read()
        raw = struct.pack('!B', 255) + raw[1:]
        os.chmod(path, os.stat(path).st_mode | stat.S_IWRITE)
        with open(path, 'w+') as f:
            f.write(raw)

        try:
            pack = self.datapackreader(pack.path)
            self.assertTrue(False, "bad version number should have thrown")
        except RuntimeError:
            pass

    def testMissingDeltabase(self):
        fakenode = self.getFakeHash()
        revisions = [("filename", fakenode, self.getFakeHash(), "content")]
        pack = self.createPack(revisions)
        chain = pack.getdeltachain("filename", fakenode)
        self.assertEquals(len(chain), 1)

    def testLargePack(self):
        """Test creating and reading from a large pack with over X entries.
        This causes it to use a 2^16 fanout table instead."""
        revisions = []
        blobs = {}
        total = SMALLFANOUTCUTOFF + 1
        for i in xrange(total):
            filename = "filename-%s" % i
            content = filename
            node = self.getHash(content)
            blobs[(filename, node)] = content
            revisions.append((filename, node, nullid, content))

        pack = self.createPack(revisions)
        if self.paramsavailable:
            self.assertEquals(pack.params.fanoutprefix, LARGEFANOUTPREFIX)

        for (filename, node), content in blobs.iteritems():
            actualcontent = pack.getdeltachain(filename, node)[0][4]
            self.assertEquals(actualcontent, content)

    # perf test off by default since it's slow
    def _testIndexPerf(self):
        random.seed(0)
        print "Multi-get perf test"
        packsizes = [
            100,
            10000,
            100000,
            500000,
            1000000,
            3000000,
        ]
        lookupsizes = [
            10,
            100,
            1000,
            10000,
            100000,
            1000000,
        ]
        for packsize in packsizes:
            revisions = []
            for i in xrange(packsize):
                filename = "filename-%s" % i
                content = "content-%s" % i
                node = self.getHash(content)
                revisions.append((filename, node, nullid, content))

            path = self.createPack(revisions).path

            # Perf of large multi-get
            import gc
            gc.disable()
            pack = self.datapackreader(path)
            for lookupsize in lookupsizes:
                if lookupsize > packsize:
                    continue
                random.shuffle(revisions)
                findnodes = [(rev[0], rev[1]) for rev in revisions]

                start = time.time()
                pack.getmissing(findnodes[:lookupsize])
                elapsed = time.time() - start
                print ("%s pack %s lookups = %0.04f" %
                       (('%s' % packsize).rjust(7),
                        ('%s' % lookupsize).rjust(7),
                        elapsed))

            print ""
            gc.enable()

        # The perf test is meant to produce output, so we always fail the test
        # so the user sees the output.
        raise RuntimeError("perf test always fails")

class datapacktests(datapacktestsbase, unittest.TestCase):
    def __init__(self, *args, **kwargs):
        datapacktestsbase.__init__(self, datapack, True)
        unittest.TestCase.__init__(self, *args, **kwargs)

class fastdatapacktests(datapacktestsbase, unittest.TestCase):
    def __init__(self, *args, **kwargs):
        datapacktestsbase.__init__(self, fastdatapack, False)
        unittest.TestCase.__init__(self, *args, **kwargs)

# TODO:
# datapack store:
# - getmissing
# - GC two packs into one

if __name__ == '__main__':
    silenttestrunner.main(__name__)
