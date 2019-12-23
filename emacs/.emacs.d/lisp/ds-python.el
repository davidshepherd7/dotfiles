
(require 'validate)
(require 'flycheck)
(require 's)
(require 'page-break-lines)

;; Automatically use python mode from "python-mode.el"
(require 'python)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(defun prettify-lambda ()
  (add-to-list 'prettify-symbols-alist '("lambda" . 955)))

(add-hook 'python-mode-hook #'prettify-symbols-mode)
(add-hook 'python-mode-hook #'prettify-lambda)
(add-hook 'python-mode-hook #'page-break-lines-mode)
(add-hook 'python-mode-hook #'flycheck-mode)

(use-package blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook #'blacken-mode))

;; Mode hooks
;; ============================================================
(add-hook 'python-mode-hook 'ds/python-keybinds)

(defun ds/python-keybinds ()
  (interactive)
  (use-local-map '()) ;; disable all keys
  (local-set-key (kbd "C-`") #'next-error)
  (local-set-key (kbd "C-¬") #'previous-error)

  ;; ;; Some things copied from python mode that were actually useful:
  ;; (local-set-key [remap delete-forward-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-backward-char] 'py-electric-backspace)
  ;; (local-set-key [remap newline] 'py-newline-and-indent)
  ;; (local-set-key [remap newline-and-indent] 'py-newline-and-indent)

  (local-set-key [tab] #'indent-for-tab-command)
  )

(defun ds/python-setup-indent ()
  (setq-local indent-tabs-mode nil)
  (setq-local python-indent-offset 4))
(add-hook 'python-mode-hook #'ds/python-setup-indent)


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
(put 'flycheck-python-pycompile-executable 'safe-local-variable (lambda (value) (equal value  "~/.pyenv/versions/3.7.4/bin/python3")))
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

