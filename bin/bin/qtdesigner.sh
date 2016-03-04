#!/bin/bash

set -o errexit
set -o nounset

exec /opt/qt5/5.3/gcc_64/bin/designer "$@"
