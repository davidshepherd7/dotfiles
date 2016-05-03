#!/bin/bash

set -o errexit
set -o nounset

emacsclient \
    -e "(set-frame-parameter (selected-frame) 'title \"helm-dmenu\")" \
    -e "(let ((helm-full-frame t)) (call-interactively #'helm-run-external-command))" \
    -e "(delete-frame)" \
    -c \
    -a "helm-dmenu-alternative.sh"
