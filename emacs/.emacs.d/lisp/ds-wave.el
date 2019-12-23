

;;; Python

(require 'flycheck)

;; (flycheck-def-args-var ds/flycheck-python-dmypy-args python-dmypy)

;; (flycheck-define-checker python-dmypy
;;   "Dmypy syntax checker.
;; Customize `flycheck-python-dmypy-args` to add specific args to default
;; executable.
;; E.g. when processing Python2 files, add \"--py2\".
;; See URL `http://mypy-lang.org/'."

;;   :command (ds/flycheck-python-dmypy-binary (eval ds/flycheck-python-dmypy-args))
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column ": error:" (message) line-end)
;;    (warning line-start (file-name) ":" line ":" column ": warning:" (message) line-end)
;;    (info line-start (file-name) ":" line ":" column ": note:" (message) line-end))
;;   :modes python-mode)

(defun ds/asterisk-conf-windows ()
  (interactive)
  (when (s-contains-p (buffer-file-name) "/asterisk-logic/")
    (conf-windows-mode)))
(add-hook 'conf-mode-hook #'ds/asterisk-conf-windows)

;; (add-to-list 'flycheck-checkers 'python-dmypy t)
