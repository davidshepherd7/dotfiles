(require 's)
(require 'f)
(require 'projectile)


(defun ds/asterisk-conf-windows ()
  (interactive)
  (when (s-contains-p (buffer-file-name) "/asterisk-logic/")
    (conf-windows-mode)))
(add-hook 'conf-mode-hook #'ds/asterisk-conf-windows)


(require 'python)

;; (defun ds/python-related-files (filename-or-path)
;;   (--filter (and (not (s-contains-p "snap" it t))
;;                  (s-contains-p (file-name-nondirectory filename-or-path) it t)
;;                  (string-equal (file-name-extension it) "py"))
;;             (projectile-current-project-files)))

;; ;; TODO(david): pick match with shortest file basename

;; (defun ds/python-test-file (filename-or-path)
;;   (--> filename-or-path
;;        (ds/python-related-files it)
;;        (--filter (s-contains-p "test_" (f-base it) t) it)
;;        (--min-by (length (f-base it)) it)
;;        (f-join (projectile-project-root) it)))

;; (defun ds/python-non-test-file (filename-or-path)
;;   (--> filename-or-path
;;        (s-replace "test_" "" it)
;;        (ds/python-related-files it)
;;        (--filter (not (s-contains-p "test_" (f-base it) t)) it)
;;        (--min-by (length (f-base it)) it)
;;        (f-join (projectile-project-root) it)
;;        ))

;; (defun ds/python-switch-to-test-file ()
;;   (interactive)
;;   (if (s-contains-p "test_" (file-name-nondirectory (buffer-file-name)))
;;       (find-file (ds/python-non-test-file (buffer-file-name)))
;;     (find-file (ds/python-test-file (buffer-file-name)))))

;; (global-unset-key (kbd "C-\\ o"))
;; (define-key python-mode-map (kbd "C-\\ o") #'ds/python-switch-to-test-file)

(defun ds/current-test-function ()
  (interactive)
  (when (s-contains-p "test_" buffer-file-name)    
    (save-excursion
      (ignore-errors
        (end-of-line)
        (re-search-backward "def test_\\|^class Test")
        (forward-char 7)
        (thing-at-point 'symbol)))))


(defun ds/money-srv-test ()
  "Run tests for a python file in money-srv"
  (interactive)
  (let* ((default-directory (or (projectile-compilation-dir) (projectile-project-root)))
         (compilation-read-command t)
         (test-file (if (s-contains-p "test_" buffer-file-name) buffer-file-name (projectile-find-matching-test buffer-file-name)))
         (relative-file (file-relative-name test-file (f-join (projectile-project-root) "money-srv")))
         (test-fn (ds/current-test-function)))
    (setq compile-command
          (concat "mm m.typecheck && ./money-srv/bin/run_tests -v "
                  (shell-quote-argument relative-file)
                  (when test-fn
                    (concat " -k " test-fn))
                  ))
    (call-interactively #'compile)))

(define-key python-mode-map  (kbd "<f6>") #'ds/money-srv-test)

(defun ds/insert-snapshot-update ()
  (interactive)
  (insert " --snapshot-partial-update"))
(define-key minibuffer-local-map (kbd "M-.") #'ds/insert-snapshot-update)


(defun ds/open-with-android-studio ()
  (interactive)
  (start-process "android-studio" nil "android-studio" (buffer-file-name)))

(defun ds/open-with-pycharm ()
  (interactive)
  (start-process "pycharm" nil "pycharm" (buffer-file-name)))

(require 'aggressive-fill-paragraph)
(defun ds/afp-in-semgrep-comment? ()
  (and (afp-inside-comment?)
       (string-match-p ".*nosemgrep.*" (afp-current-line))))
(add-to-list 'afp-suppress-fill-pfunction-list #'ds/afp-in-semgrep-comment?)


(defvar ds/venvs)
(setq ds/venvs
      (list
       (cons "monorepo/udp_forwarder" "/home/david/code/monorepo/udp_forwarder/.udp_forwarder-venv")
       (cons "monorepo/wavemodem" "/home/david/code/monorepo/wavemodem/.wavemodem-venv")
       (cons "monorepo/ussd" "/home/david/code/monorepo/ussd/.ussd-venv")
       (cons "monorepo/money-srv" "/home/david/code/monorepo/money-srv/.money-srv-venv")
       (cons "monorepo/tools/wavecli" "/home/david/code/monorepo/tools/wavecli/.wavecli-venv")
       (cons "monorepo/wavesms" "/home/david/code/monorepo/wavesms/.wavesms-venv"  )
       (cons "monorepo" "/home/david/code/monorepo/.root-venv")))

(defun ds/find-venv ()
  (cdr (--first (s-contains-p (car it) default-directory) ds/venvs)))

(defun ds/add-wave-venvs-to-path(&rest args)
  (let* ((venv (ds/find-venv))
         (exec-path (if venv (cons venv exec-path) exec-path)))
    (apply args)))

;; (advice-add #'flycheck-checker-shell-command :around #'ds/add-wave-venvs-to-path)
;; (advice-add #'flycheck-verify-setup :around #'ds/add-wave-venvs-to-path)

(require 'apheleia)
(add-to-list 'apheleia-formatters (list 'wave-black "~/code/monorepo/.root-venv/bin/black" "-"))
(add-to-list 'apheleia-mode-alist (cons 'python-mode 'wave-black))

(defvar ds-disable-font-lock-files '())
(setq ds-disable-font-lock-files '("configvars.py"))

(defun ds/disable-font-lock-large-files ()
  (interactive)
  (when (-some
         (lambda (x) (s-equals-p (f-filename (buffer-file-name)) x))
         ds-disable-font-lock-files)
    (font-lock-mode -1)))

(add-to-list 'python-mode-hook #'ds/disable-font-lock-large-files)


(add-to-list 'directory-abbrev-alist '("^/money-srv" . "/home/david/code/monorepo/money-srv"))
