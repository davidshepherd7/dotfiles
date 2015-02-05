

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

(when emacs244?
  (add-hook 'python-mode-hook 'prettify-symbols-mode)
  (add-hook 'python-mode-hook 'prettify-lambda)
  )

;; Setup jedi mode (python autocompletion)
;; ============================================================

;; ;; Enable it, but only for autocompletion so we don't get all the keybind
;; ;; junk.
;; (add-hook 'python-mode-hook 'jedi:ac-setup)

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
  (local-set-key (kbd "<f5>") 'recompile)
  (local-set-key (kbd "S-<f5>") 'python-run-file)

  ;; ;; Some things copied from python mode that were actually useful:
  ;; (local-set-key [remap delete-forward-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-char] 'py-electric-delete)
  ;; (local-set-key [remap delete-backward-char] 'py-electric-backspace)
  ;; (local-set-key [remap newline] 'py-newline-and-indent)
  ;; (local-set-key [remap newline-and-indent] 'py-newline-and-indent)

  ;; (local-set-key [tab] 'py-indent-line)
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
