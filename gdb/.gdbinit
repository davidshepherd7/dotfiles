python
import sys

sys.path.insert(0, '/usr/share/gcc-4.8/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)

sys.path.insert(0, '/home/david/.gdb_pretty_printers/Boost-Pretty-Printer')
import boost.latest
boost.register_printers()

end
