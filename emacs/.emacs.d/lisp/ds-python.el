

;; Automatically use python mode from "python-mode.el"
(require 'python)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ;; Turn "lambda " into a lambda symbol
;; (require 'lambda-mode)
;; (add-hook 'python-mode-hook 'lambda-mode 1)
;; (setq lambda-regex "lambda ")

(defun prettify-lambda ()
  (add-to-list 'prettify-symbols-alist '("lambda" . 955)))

(when ds/emacs-up-to-date?
  (add-hook 'python-mode-hook 'prettify-symbols-mode)
  (add-hook 'python-mode-hook 'prettify-lambda)
  )

;; Autocompletion
(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)
  )
(use-package company-anaconda
  :ensure t)


(add-hook 'python-mode-hook #'page-break-lines-mode)

;; Build/test/check functions
;; ============================================================

(setq nose-command "nosetests3 -d --all-modules --processes=8")

(defun nose (input-command)
  "Run nosetests."
  (interactive (list (read-string "Nose command: " nose-command)))
  (setq nose-command input-command)
  (save-buffer)
  (compilation-start input-command t (lambda (x) "*nose*")))

(defun renose ()
  "Run nose with previous/default args."
  (interactive)
  (nose nose-command))

(setq stored-python-command nil)

(defun python-command-default (buffer-name)
  ;; Note: we need to run it via bash (and with -l) to get the .bashrc
  ;; config options such as the correct $PYTHONPATH value.
  ;; Use ipython classic and no colors to get jump to errors working.
  (concat "bash -l -c \"ipython --classic --colors='nocolor' "
          buffer-name "\""))

(defun python-run-file (python-command)
  "Run a file in python."
  (interactive (list (read-string "Python command: "
                                  (python-command-default (buffer-name)))))
  (save-buffer)
  (compilation-start python-command t (lambda (x) "*python-run*"))
  (setq stored-python-command python-command))


(defun python-rerun-file ()
  (interactive)
  (if (not (eq nil stored-python-command))
      (python-run-file stored-python-command)
    (error "No previous python command.")))

;; Mode hooks
;; ============================================================
(add-hook 'python-mode-hook 'py-keybinds)

(defun py-keybinds ()
  (interactive)
  (use-local-map '()) ;; disable all keys
  (local-set-key (kbd "C-`") 'next-error)
  (local-set-key (kbd "C-¬") 'previous-error)
  (local-set-key (kbd "<f6>") 'renose)

  ;; ;; Some things copied from python mode that were actually useful:
  ;; (local-set-key [remap delete-forward-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-backward-char] 'py-electric-backspace)
  ;; (local-set-key [remap newline] 'py-newline-and-indent)
  ;; (local-set-key [remap newline-and-indent] 'py-newline-and-indent)

  (local-set-key [tab] 'indent-for-tab-command)
  )

(defun yas-advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas-try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas-fallback-behavior nil))
             (unless (and (called-interactively-p 'any)
                          (yas-expand))
               ad-do-it)))))

;; (yas-advise-indent-function 'py-indent-line)


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

(defun toggle-generate-results ()
  "Toggle the \"generate_results\" bool. Assumes there is only
  one in the buffer, if there is more than one the last is
  toggled."
  (interactive)
  (save-excursion
    (end-of-buffer)
    (search-backward "generate_results = ")
    (python-toggle-bool)))


;; Auto pep8 buffer
;; ============================================================

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


;; Electric newlines
(defun ds/enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil
        if not inside any parens."
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun ds/python-electric-newline ()
  (let ((paren (ds/enclosing-paren)))
    (if (not (or (eq paren ?\{)
                 (eq paren ?\[)
                 (eq paren ?\()
                 (looking-back "\\blambda\\b.*")))
        'after
      nil)))

(defun ds/setup-python-electric-layout ()
  (make-local-variable 'electric-layout-rules)
  (add-to-list 'electric-layout-rules (cons ?: #'ds/python-electric-newline))
  )

(electric-layout-mode 1)
(add-hook 'python-mode-hook #'ds/setup-python-electric-layout)


(eval-after-load 'flycheck
  (validate-setq flycheck-flake8-maximum-line-length 120))
