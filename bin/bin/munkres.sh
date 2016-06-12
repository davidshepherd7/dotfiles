#!/bin/bash

set -o errexit
set -o nounset

evince "$(find "$HOME/Dropbox/education/topology/" -name '*pp*' | sort -V | tail -n1)"
