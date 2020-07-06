
(require 's)


(defun ds/asterisk-conf-windows ()
  (interactive)
  (when (s-contains-p (buffer-file-name) "/asterisk-logic/")
    (conf-windows-mode)))
(add-hook 'conf-mode-hook #'ds/asterisk-conf-windows)


(require 'python)

(defun ds/python-related-files (filename-or-path)
  (--filter (and (not (s-contains-p "snap" it t))
                 (s-contains-p (file-name-nondirectory filename-or-path) it t)
                 (string-equal (file-name-extension it) "py"))
            (projectile-current-project-files)))

(defun ds/python-test-file (filename-or-path)
  (f-join (projectile-project-root)
          (--first (s-contains-p "test" it t)
                   (ds/python-related-files filename-or-path))))

(defun ds/python-non-test-file (filename-or-path)
  (f-join (projectile-project-root)
          (--first (not (s-contains-p "test" it t))
                   (ds/python-related-files (s-replace "test_" "" filename-or-path)))))

(defun ds/python-switch-to-test-file ()
  (interactive)
  (if (s-contains-p "test_" (file-name-nondirectory (buffer-file-name)))
      (find-file (ds/python-non-test-file (buffer-file-name)))
    (find-file (ds/python-test-file (buffer-file-name)))))

(global-unset-key (kbd "C-\\ o"))
(define-key python-mode-map (kbd "C-\\ o") #'ds/python-switch-to-test-file)


(defun ds/money-srv-test ()
  "Run tests for a python file in money-srv"
  (interactive)
  (let* ((default-directory (or (projectile-compilation-dir) (projectile-project-root)))
         (compilation-read-command t)
         (test-file (if (s-contains-p "test_" buffer-file-name)
                        buffer-file-name
                      (ds/python-test-file buffer-file-name)))
         (relative-file (file-relative-name test-file (f-join (projectile-project-root) "money-srv"))))
    (setq compile-command
          (concat "mm m.typecheck && ./money-srv/bin/run_tests " (shell-quote-argument relative-file)))
    (call-interactively #'compile)))

(define-key python-mode-map  (kbd "<f6>") #'ds/money-srv-test)
