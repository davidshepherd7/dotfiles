
(when ds/emacs-up-to-date?
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))

;; Use double semi-colon for emacs lisp (default seems to be single).
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";;"
                                            comment-end "")))
