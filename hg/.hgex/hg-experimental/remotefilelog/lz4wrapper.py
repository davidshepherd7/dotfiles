from mercurial import error
from mercurial.i18n import _

import lz4

def missing(*args, **kwargs):
    raise error.Abort(_('remotefilelog extension requires lz4 support'))

lz4compress = lzcompresshc = lz4decompress = missing

try:
    # newer python-lz4 has these functions deprecated as top-level ones,
    # so we are trying to import from lz4.block first
    def _compressHC(*args, **kwargs):
        return lz4.block.compress(*args, mode='high_compression', **kwargs)
    lzcompresshc = _compressHC
    lz4compress = lz4.block.compress
    lz4decompress = lz4.block.decompress
except (AttributeError, ImportError):
    # ImportError is possible due to DemandImport
    try:
        lzcompresshc = lz4.compressHC
        lz4compress = lz4.compress
        lz4decompress = lz4.decompress
    except (AttributeError, ImportError):
        pass
