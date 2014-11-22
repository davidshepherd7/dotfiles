#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-


(setq require-final-newline 'visit)

(defun indent-files (files)
  (dolist (file files)
    (find-file file)

    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))

    ;; kill \r (ie windows newlines)
    (while (search-forward "\r" nil t) (replace-match ""))

    (save-buffer)
    (kill-buffer)))

;; run on command line args given to script
(indent-files argv)

;; clean out the cli args so that emacs doesn't barf on anything begining
;; with --
(setq argv nil)

;; done
(kill-emacs 0)
