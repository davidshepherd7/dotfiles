
(require 'validate)
(require 'flycheck)
(require 's)
(require 'f)
(require 'page-break-lines)
(require 'use-package)
(require 'projectile)

;; Automatically use python mode from "python-mode.el"
(require 'python)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(defun prettify-lambda ()
  (setq prettify-symbols-alist '(("lambda" . 955))))

(add-hook 'python-mode-hook #'prettify-symbols-mode)
(add-hook 'python-mode-hook #'prettify-lambda)
(add-hook 'python-mode-hook #'page-break-lines-mode)
(add-hook 'python-mode-hook #'flycheck-mode)

(use-package blacken
  :config
  (add-hook 'python-mode-hook #'blacken-mode))


;; (use-package jedi
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)

;;   (validate-setq jedi:complete-on-dot t)


;;   (define-key jedi-mode-map (kbd "C-c ?") nil)
;;   (define-key jedi-mode-map (kbd "C-c .") nil)
;;   (define-key jedi-mode-map (kbd "C-c ,") nil)
;;   (define-key jedi-mode-map (kbd "C-c /") nil)


;;   (define-key jedi-mode-map (kbd "M-.") #'jedi:goto-definition)
;;   (define-key jedi-mode-map (kbd "M-,") #'jedi:goto-definition-pop-marker)
;;   (define-key jedi-mode-map (kbd "C-?") #'jedi:show-doc)

;;   (add-hook 'jedi:doc-hook #'evil-emacs-state)
;;   )

;; Mode hooks
;; ============================================================
(add-hook 'python-mode-hook 'ds/python-keybinds)

(defun ds/python-keybinds ()
  (interactive)
  ;; (use-local-map '()) ;; disable all keys
  (local-set-key (kbd "C-`") #'next-error)
  (local-set-key (kbd "C-¬") #'previous-error)

  ;; ;; Some things copied from python mode that were actually useful:
  ;; (local-set-key [remap delete-forward-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-backward-char] 'py-electric-backspace)
  ;; (local-set-key [remap newline] 'py-newline-and-indent)
  ;; (local-set-key [remap newline-and-indent] 'py-newline-and-indent)

  (local-set-key [tab] #'indent-for-tab-command)
  (local-unset-key (kbd "<backtab>"))
  )

(defun ds/python-setup-indent ()
  (setq-local indent-tabs-mode nil)
  (setq-local python-indent-offset 4))
(add-hook 'python-mode-hook #'ds/python-setup-indent)


(setq python-font-lock-keywords-maximum-decoration
      (append python-font-lock-keywords-maximum-decoration
              '(;; this is the full string.
                ;; group 1 is the quote type and a closing quote is matched
                ;; group 2 is the string part
                ("f\\(['\"]\\{1,3\\}\\)\\(.+?\\)\\1"
                 ;; these are the {keywords}
                 ("{\\([^}]*?\\)}"
                  ;; Pre-match form
                  (progn (goto-char (match-beginning 0)) (match-end 0))
                  ;; Post-match form
                  (goto-char (match-end 0))
                  ;; face for this match
                  (1 font-lock-variable-name-face t))))))

;; Function to toggle true/false
;; ============================================================

(defun python-toggle-bool ()
  "Toggle next bool from point (True or False)."
  (interactive)
  (save-excursion
    ;; Find next true or false
    (re-search-forward "True\\|False" nil nil)
    (backward-word)

    ;; Store the word
    (let ((t-or-f-string (buffer-substring (point) (save-excursion (forward-word) (point)))))

      ;; Delete and replace with other word
      (kill-word 1)
      (if (equal t-or-f-string "True")
          (insert "False")
        (insert "True")))))


(defun auto-pep8-buffer ()
  "Run autopep8 on the current buffer"
  (interactive)

  ;; E701 is multiple statements on one line (colon), i.e. one line
  ;; functions

  ;; E26 is "Format block comments" - destroys commented out code so don't
  ;; use

  ;; E226 is whitespace around *, don't always want this in dense maths
  (progn
    (shell-command-on-region
     (point-min)
     (point-max)
     "autopep8 --aggressive --ignore=E226 -ignore=E701 -"
     nil t)))


(defun ds/shell-to-python ()
  (interactive)
  (let ((shellified (-->
                     (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                     (s-trim it)
                     (s-split " " it)
                     (--map (s-replace "\"" "" it) it)
                     (-map (lambda (x)
                             (if (s-matches? "^\\$" x)
                                 (s-replace "$" "" x)
                               (s-concat "\"" x "\""))) it)
                     (s-join ", " it)
                     (s-concat "subprocess.check_call([" it "])"))))
    (delete-region (point-at-bol) (point-at-eol))
    (insert shellified)))


(validate-setq flycheck-flake8-maximum-line-length 120)
(validate-setq flycheck-python-pycompile-executable "pyenv-python3")

;; Shut up the warnining about my pycompile checker binary
(put 'flycheck-python-pycompile-executable 'safe-local-variable (lambda (value) (equal value  "/home/david/.pyenv/versions/3.7.4/bin/python3")))
(put 'flycheck-python-mypy-executable 'safe-local-variable (lambda (value) t))
(put 'flycheck-ds-python-dmypy-executable 'safe-local-variable (lambda (value) t))

;; ;; Load lsp, but turn off most features by default (they slow things down and I
;; ;; don't need them).
;; (require 'lsp)
;; (validate-setq lsp-enable-completion-at-point nil)
;; (validate-setq lsp-enable-symbol-highlighting nil)
;; (validate-setq lsp-enable-indentation nil)
;; (validate-setq lsp-enable-on-type-formatting nil)
;; (validate-setq lsp-enable-xref t)
;; (validate-setq lsp-before-save-edits nil)
;; (validate-setq lsp-prefer-flymake :none)


;; (require 'lsp-pyls)
;; (add-hook 'python-mode-hook #'lsp)

;; ;; HACK: use the correct pyls for Wave money-srv, I should make something more
;; ;; general...
;; (validate-setq lsp-pyls-server-command '("env" "PATH=/home/david/code/monorepo/money-srv/.money-srv-venv/bin:/home/david/.pyenv/versions/3.7.4/bin:$PATH" "pyls"))

;; ;; (validate-setq lsp-pyls-plugins-jedi-completion-enabled t)
;; ;; (validate-setq lsp-pyls-plugins-jedi-hover-enabled t)
;; ;; (validate-setq lsp-pyls-plugins-jedi-references-enabled t)
;; ;; (validate-setq lsp-pyls-plugins-jedi-signature-help-enabled t)
;; ;; (validate-setq lsp-pyls-plugins-jedi-symbols-enabled t)

;; ;; Disable most of the extra linters etc for pyls (I'm happy with flycheck for now)
;; (validate-setq lsp-pyls-plugins-mccabe-enabled nil)
;; (validate-setq lsp-pyls-plugins-pylint-enabled nil)
;; (validate-setq lsp-pyls-plugins-pycodestyle-enabled nil)
;; (validate-setq lsp-pyls-plugins-pyflakes-enabled nil)
;; (validate-setq lsp-pyls-plugins-yapf-enabled nil)


(flycheck-define-checker ds-python-dmypy
  "Mypy syntax and type checker daemon.

See URL `http://mypy-lang.org/'."
  :command ("dmypy"
            "run"
            "--"
            "src" "../wavelib" "unittests" "conftest.py"
            )
  :error-patterns
  ((error line-start (file-name) ":" line (optional ":" column)
          ": error:" (message) line-end)
   (warning line-start (file-name) ":" line (optional ":" column)
            ": warning:" (message) line-end)
   (info line-start (file-name) ":" line (optional ":" column)
         ": note:" (message) line-end))
  :modes python-mode
  ;; Ensure the file is saved, to work around
  ;; https://github.com/python/mypy/issues/4746.
  :predicate flycheck-buffer-saved-p
  :working-directory flycheck-mypy--find-project-root)
(add-to-list 'flycheck-checkers 'ds-python-dmypy)

(flycheck-define-checker ds/python-pyflakes
  "A Python syntax and style checker using the pyflakes utility.
To override the path to the pyflakes executable, set
`flycheck-python-pyflakes-executable'.
See URL `http://pypi.python.org/pypi/pyflakes'."
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

;; It's too annoying for now...
;; (add-to-list 'flycheck-checkers 'ds/python-pyflakes)

;; Magic imports
;; ============================================================


;; TODO: add stdlib modules to paths

;; TODO: add directories to possible paths

;; TODO: complete possible symbols by regexing files?

;; TODO: extend existing import lines? Or just use isort for that?

(defun ds/import (insert-here)
  "Insert an import statement at the start of the file."
  (interactive "P")
  (let* ((files (--> (projectile-current-project-files)
                     (-filter (lambda (path) (s-ends-with-p ".py" path)) it)))
         (default-input (when (symbol-at-point) (symbol-name (symbol-at-point))))
         (file (completing-read "import file: " files nil nil default-input))
         (individual-symbol (read-from-minibuffer "symbol: "))
         (import-statement (ds/path-to-import-statement file individual-symbol)))
    (if insert-here
        (insert (s-concat import-statement "\n"))
      (ds/insert-as-import import-statement))))

(defun ds/path-to-import-statement (path symbol)
  (let* ((module (--> path
                      (s-trim it)
                      (f-join (projectile-project-root) it)
                      (file-relative-name it (projectile-project-root))
                      (s-chop-prefix "money-srv/src/" it)
                      (s-chop-prefix "wavesms/src/" it)
                      (s-chop-prefix "wavemodem/src/" it)
                      (s-chop-prefix "wavelib/" it)
                      (s-chop-suffix ".py" it)
                      (s-replace "/" "." it)
                      )))
    (if (equal symbol "")
        (s-concat "import " module)
      (s-concat "from " module " import " symbol))))

(defun ds/pick-import-location ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "import\\|defun\\|class")
    (beginning-of-line)
    (point)))

(defun ds/insert-as-import (line)
  (save-excursion
    (goto-char (ds/pick-import-location))
    (insert (s-concat line "\n"))
    (message "Added import: %s" line)))

(define-key python-mode-map (kbd "C-,") #'ds/import)


(defun ds/kwargs-to-dict (start end)
  "Convert selected python kwargs to a dictionary"
  (interactive (list (region-beginning) (region-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert "{")
      (while (re-search-forward "\\([a-zA-Z_]*\\)=" nil t)
        (replace-match "\"\\1\": " nil nil))
      (goto-char (point-max))
      (insert "}"))))

(defun ds/dict-to-kwargs (start end)
  "Convert selected dictionary into kwargs (must select entire an dict)"
  (interactive (list (region-beginning) (region-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)

      (goto-char (point-min))
      (re-search-forward "{" nil)
      (replace-match "" nil nil)

      (goto-char (point-min))
      (while (re-search-forward "\"\\([a-zA-Z_0-9]*\\)\":" nil t)
        (replace-match "\\1=" nil nil))

      (goto-char (point-max))
      (re-search-backward "},?" nil)
      (replace-match "" nil nil)
      )))
