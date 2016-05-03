set history save

set history filename ~/.gdb_history

add-auto-load-safe-path /home/david/code/emacs/src/.gdbinit

python
import sys

sys.path.insert(0, '/usr/share/gcc-4.8/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)

sys.path.insert(0, '/home/david/.gdb_pretty_printers/Boost-Pretty-Printer')
import boost.latest
boost.register_printers()

sys.path.insert(0, '/home/david/.gdb_pretty_printers/qt4_printers')
from qt4 import register_qt4_printers
register_qt4_printers (None)

end
