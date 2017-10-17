# shallowutil.py -- remotefilelog utilities
#
# Copyright 2014 Facebook, Inc.
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.
from __future__ import absolute_import

import errno, hashlib, os, stat, struct, tempfile
from mercurial import filelog, revlog, util, error
from mercurial.i18n import _

from . import constants

if os.name != 'nt':
    import grp

def interposeclass(container, classname):
    '''Interpose a class into the hierarchies of all loaded subclasses. This
    function is intended for use as a decorator.

      import mymodule
      @replaceclass(mymodule, 'myclass')
      class mysubclass(mymodule.myclass):
          def foo(self):
              f = super(mysubclass, self).foo()
              return f + ' bar'

    Existing instances of the class being replaced will not have their
    __class__ modified, so call this function before creating any
    objects of the target type. Note that this doesn't actually replace the
    class in the module -- that can cause problems when using e.g. super()
    to call a method in the parent class. Instead, new instances should be
    created using a factory of some sort that this extension can override.
    '''
    def wrap(cls):
        oldcls = getattr(container, classname)
        oldbases = (oldcls,)
        newbases = (cls,)
        for subcls in oldcls.__subclasses__():
            if subcls is not cls:
                assert subcls.__bases__ == oldbases
                subcls.__bases__ = newbases
        return cls
    return wrap

def getcachekey(reponame, file, id):
    pathhash = hashlib.sha1(file).hexdigest()
    return os.path.join(reponame, pathhash[:2], pathhash[2:], id)

def getlocalkey(file, id):
    pathhash = hashlib.sha1(file).hexdigest()
    return os.path.join(pathhash, id)

def getcachepath(ui, allowempty=False):
    cachepath = ui.config("remotefilelog", "cachepath")
    if not cachepath:
        if allowempty:
            return None
        else:
            raise error.Abort(_("could not find config option "
                                "remotefilelog.cachepath"))
    return util.expandpath(cachepath)

def getcachepackpath(repo, category):
    cachepath = getcachepath(repo.ui)
    if category != constants.FILEPACK_CATEGORY:
        return os.path.join(cachepath, repo.name, 'packs', category)
    else:
        return os.path.join(cachepath, repo.name, 'packs')

def getlocalpackpath(base, category):
    return os.path.join(base, 'packs', category)

def createrevlogtext(text, copyfrom=None, copyrev=None):
    """returns a string that matches the revlog contents in a
    traditional revlog
    """
    meta = {}
    if copyfrom or text.startswith('\1\n'):
        if copyfrom:
            meta['copy'] = copyfrom
            meta['copyrev'] = copyrev
        text = filelog.packmeta(meta, text)

    return text

def parsemeta(text):
    """parse mercurial filelog metadata"""
    meta, size = filelog.parsemeta(text)
    if text.startswith('\1\n'):
        s = text.index('\1\n', 2)
        text = text[s + 2:]
    return meta or {}, text

def _parsepackmeta(metabuf):
    """parse datapack meta, bytes (<metadata-list>) -> dict

    The dict contains raw content - both keys and values are strings.
    Upper-level business may want to convert some of them to other types like
    integers, on their own.

    raise ValueError if the data is corrupted
    """
    metadict = {}
    offset = 0
    buflen = len(metabuf)
    while buflen - offset >= 3:
        key = metabuf[offset]
        offset += 1
        metalen = struct.unpack_from('!H', metabuf, offset)[0]
        offset += 2
        if offset + metalen > buflen:
            raise ValueError('corrupted metadata: incomplete buffer')
        value = metabuf[offset:offset + metalen]
        metadict[key] = value
        offset += metalen
    if offset != buflen:
        raise ValueError('corrupted metadata: redundant data')
    return metadict

def _buildpackmeta(metadict):
    """reverse of _parsepackmeta, dict -> bytes (<metadata-list>)

    The dict contains raw content - both keys and values are strings.
    Upper-level business may want to serialize some of other types (like
    integers) to strings before calling this function.

    raise ProgrammingError when metadata key is illegal, or ValueError if
    length limit is exceeded
    """
    metabuf = ''
    for k, v in sorted((metadict or {}).iteritems()):
        if len(k) != 1:
            raise error.ProgrammingError('packmeta: illegal key: %s' % k)
        if len(v) > 0xfffe:
            raise ValueError('metadata value is too long: 0x%x > 0xfffe'
                             % len(v))
        metabuf += k
        metabuf += struct.pack('!H', len(v))
        metabuf += v
    # len(metabuf) is guaranteed representable in 4 bytes, because there are
    # only 256 keys, and for each value, len(value) <= 0xfffe.
    return metabuf

_metaitemtypes = {
    constants.METAKEYFLAG: (int, long),
    constants.METAKEYSIZE: (int, long),
}

def buildpackmeta(metadict):
    """like _buildpackmeta, but typechecks metadict and normalize it.

    This means, METAKEYSIZE and METAKEYSIZE should have integers as values,
    and METAKEYFLAG will be dropped if its value is 0.
    """
    newmeta = {}
    for k, v in (metadict or {}).iteritems():
        expectedtype = _metaitemtypes.get(k, (bytes,))
        if not isinstance(v, expectedtype):
            raise error.ProgrammingError('packmeta: wrong type of key %s' % k)
        # normalize int to binary buffer
        if int in expectedtype:
            # optimization: remove flag if it's 0 to save space
            if k == constants.METAKEYFLAG and v == 0:
                continue
            v = int2bin(v)
        newmeta[k] = v
    return _buildpackmeta(newmeta)

def parsepackmeta(metabuf):
    """like _parsepackmeta, but convert fields to desired types automatically.

    This means, METAKEYFLAG and METAKEYSIZE fields will be converted to
    integers.
    """
    metadict = _parsepackmeta(metabuf)
    for k, v in metadict.iteritems():
        if k in _metaitemtypes and int in _metaitemtypes[k]:
            metadict[k] = bin2int(v)
    return metadict

def int2bin(n):
    """convert a non-negative integer to raw binary buffer"""
    buf = bytearray()
    while n > 0:
        buf.insert(0, n & 0xff)
        n >>= 8
    return bytes(buf)

def bin2int(buf):
    """the reverse of int2bin, convert a binary buffer to an integer"""
    x = 0
    for b in bytearray(buf):
        x <<= 8
        x |= b
    return x

def parsesizeflags(raw):
    """given a remotefilelog blob, return (headersize, rawtextsize, flags)

    see remotefilelogserver.createfileblob for the format.
    raise RuntimeError if the content is illformed.
    """
    flags = revlog.REVIDX_DEFAULT_FLAGS
    size = None
    try:
        index = raw.index('\0')
        header = raw[:index]
        if header.startswith('v'):
            # v1 and above, header starts with 'v'
            if header.startswith('v1\n'):
                for s in header.split('\n'):
                    if s.startswith(constants.METAKEYSIZE):
                        size = int(s[len(constants.METAKEYSIZE):])
                    elif s.startswith(constants.METAKEYFLAG):
                        flags = int(s[len(constants.METAKEYFLAG):])
            else:
                raise RuntimeError('unsupported remotefilelog header: %s'
                                   % header)
        else:
            # v0, str(int(size)) is the header
            size = int(header)
    except ValueError:
        raise RuntimeError("unexpected remotefilelog header: illegal format")
    if size is None:
        raise RuntimeError("unexpected remotefilelog header: no size found")
    return index + 1, size, flags

def buildfileblobheader(size, flags, version=None):
    """return the header of a remotefilelog blob.

    see remotefilelogserver.createfileblob for the format.
    approximately the reverse of parsesizeflags.

    version could be 0 or 1, or None (auto decide).
    """
    # choose v0 if flags is empty, otherwise v1
    if version is None:
        version = int(bool(flags))
    if version == 1:
        header = ('v1\n%s%d\n%s%d'
                  % (constants.METAKEYSIZE, size,
                     constants.METAKEYFLAG, flags))
    elif version == 0:
        if flags:
            raise error.ProgrammingError('fileblob v0 does not support flag')
        header = '%d' % size
    else:
        raise error.ProgrammingError('unknown fileblob version %d' % version)
    return header

def ancestormap(raw):
    offset, size, flags = parsesizeflags(raw)
    start = offset + size

    mapping = {}
    while start < len(raw):
        divider = raw.index('\0', start + 80)

        currentnode = raw[start:(start + 20)]
        p1 = raw[(start + 20):(start + 40)]
        p2 = raw[(start + 40):(start + 60)]
        linknode = raw[(start + 60):(start + 80)]
        copyfrom = raw[(start + 80):divider]

        mapping[currentnode] = (p1, p2, linknode, copyfrom)
        start = divider + 1

    return mapping

def readfile(path):
    f = open(path, 'rb')
    try:
        result = f.read()

        # we should never have empty files
        if not result:
            os.remove(path)
            raise IOError("empty file: %s" % path)

        return result
    finally:
        f.close()


def unlinkfile(filepath):
    if os.name == 'nt':
        # On Windows, os.unlink cannnot delete readonly files
        os.chmod(filepath, stat.S_IWUSR)

    os.unlink(filepath)


def renamefile(source, destination):
    if os.name == 'nt':
        # On Windows, os.rename cannot rename readonly files
        # and cannot overwrite destination if it exists
        os.chmod(source, stat.S_IWUSR)
        if os.path.isfile(destination):
            os.chmod(destination, stat.S_IWUSR)
            os.unlink(destination)

    os.rename(source, destination)


def writefile(path, content, readonly=False):
    dirname, filename = os.path.split(path)
    if not os.path.exists(dirname):
        try:
            os.makedirs(dirname)
        except OSError as ex:
            if ex.errno != errno.EEXIST:
                raise

    fd, temp = tempfile.mkstemp(prefix='.%s-' % filename, dir=dirname)
    os.close(fd)

    try:
        f = util.posixfile(temp, 'wb')
        f.write(content)
        f.close()

        if readonly:
            mode = 0o444
        else:
            # tempfiles are created with 0o600, so we need to manually set the
            # mode.
            oldumask = os.umask(0)
            # there's no way to get the umask without modifying it, so set it
            # back
            os.umask(oldumask)
            mode = ~oldumask

        renamefile(temp, path)
        os.chmod(path, mode)
    except Exception:
        try:
            unlinkfile(temp)
        except OSError:
            pass
        raise

def sortnodes(nodes, parentfunc):
    """Topologically sorts the nodes, using the parentfunc to find
    the parents of nodes."""
    nodes = set(nodes)
    childmap = {}
    parentmap = {}
    roots = []

    # Build a child and parent map
    for n in nodes:
        parents = [p for p in parentfunc(n) if p in nodes]
        parentmap[n] = set(parents)
        for p in parents:
            childmap.setdefault(p, set()).add(n)
        if not parents:
            roots.append(n)

    roots.sort()
    # Process roots, adding children to the queue as they become roots
    results = []
    while roots:
        n = roots.pop(0)
        results.append(n)
        if n in childmap:
            children = childmap[n]
            for c in children:
                childparents = parentmap[c]
                childparents.remove(n)
                if len(childparents) == 0:
                    # insert at the beginning, that way child nodes
                    # are likely to be output immediately after their
                    # parents.  This gives better compression results.
                    roots.insert(0, c)

    return results

def readexactly(stream, n):
    '''read n bytes from stream.read and abort if less was available'''
    s = stream.read(n)
    if len(s) < n:
        raise error.Abort(_("stream ended unexpectedly"
                           " (got %d bytes, expected %d)")
                          % (len(s), n))
    return s

def readunpack(stream, fmt):
    data = readexactly(stream, struct.calcsize(fmt))
    return struct.unpack(fmt, data)

def readpath(stream):
    rawlen = readexactly(stream, constants.FILENAMESIZE)
    pathlen = struct.unpack(constants.FILENAMESTRUCT, rawlen)[0]
    return readexactly(stream, pathlen)

def readnodelist(stream):
    rawlen = readexactly(stream, constants.NODECOUNTSIZE)
    nodecount = struct.unpack(constants.NODECOUNTSTRUCT, rawlen)[0]
    for i in xrange(nodecount):
        yield readexactly(stream, constants.NODESIZE)

def readpathlist(stream):
    rawlen = readexactly(stream, constants.PATHCOUNTSIZE)
    pathcount = struct.unpack(constants.PATHCOUNTSTRUCT, rawlen)[0]
    for i in xrange(pathcount):
        yield readpath(stream)

def getgid(groupname):
    try:
        gid = grp.getgrnam(groupname).gr_gid
        return gid
    except KeyError:
        return None

def setstickygroupdir(path, gid, warn=None):
    if gid is None:
        return
    try:
        os.chown(path, -1, gid)
        os.chmod(path, 0o2775)
    except (IOError, OSError) as ex:
        if warn:
            warn(_('unable to chown/chmod on %s: %s\n') % (path, ex))

def mkstickygroupdir(ui, path):
    """Creates the given directory (if it doesn't exist) and give it a
    particular group with setgid enabled."""
    gid = None
    groupname = ui.config("remotefilelog", "cachegroup")
    if groupname:
        gid = getgid(groupname)
        if gid is None:
            ui.warn(_('unable to resolve group name: %s\n') % groupname)

    # we use a single stat syscall to test the existence and mode / group bit
    st = None
    try:
        st = os.stat(path)
    except OSError:
        pass

    if st:
        # exists
        if (st.st_mode & 0o2775) != 0o2775 or st.st_gid != gid:
            # permission needs to be fixed
            setstickygroupdir(path, gid, ui.warn)
        return

    oldumask = os.umask(0o002)
    try:
        missingdirs = [path]
        path = os.path.dirname(path)
        while path and not os.path.exists(path):
            missingdirs.append(path)
            path = os.path.dirname(path)

        for path in reversed(missingdirs):
            os.mkdir(path)

        for path in missingdirs:
            setstickygroupdir(path, gid, ui.warn)
    finally:
        os.umask(oldumask)
