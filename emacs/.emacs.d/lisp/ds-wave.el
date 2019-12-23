
(require 's)


(defun ds/asterisk-conf-windows ()
  (interactive)
  (when (s-contains-p (buffer-file-name) "/asterisk-logic/")
    (conf-windows-mode)))
(add-hook 'conf-mode-hook #'ds/asterisk-conf-windows)


(defun ds/money-srv-test ()
  "Run tests for a python file in money-srv"
  (interactive)
  (let* ((default-directory (or (projectile-compilation-dir) (projectile-project-root)))
         (file-name (file-relative-name buffer-file-name (f-join (projectile-project-root) "money-srv")))
         (compilation-read-command t))
    (when (not (s-contains? "test_" buffer-file-name))
      (error "This doesn't look like a test file"))
    (setq compile-command (concat "mm m.typecheck && ./money-srv/bin/run_tests " (shell-quote-argument file-name)))
    (call-interactively #'compile)))
