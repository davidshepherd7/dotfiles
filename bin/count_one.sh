#!/bin/bash

set -o errexit
set -o nounset

find "$1" -type f | wc -l
