<?php

phutil_register_library('fb-hgext-arcanist', __FILE__);

// When running under HHVM, reset xdebug.max_nesting_level to 0
// This is unfortunately set to PHP_INT_MAX in
// libphutil/scripts/__init_script__.php
//
// With HHVM, setting xdebug.max_nesting_level to a non-zero value
// automatically turns on XDebug profiling.  This makes many things
// substantially slower, and appears to even affect non-PHP children
// processes spawned by this process.
if (defined('HHVM_VERSION')) {
  ini_set('xdebug.max_nesting_level', 0);
}
