

;; Automatically use python mode from "python-mode.el"
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Turn "lambda " into a lambda symbol
(require 'lambda-mode)
(add-hook 'python-mode-hook 'lambda-mode 1)
(setq lambda-regex "lambda ")

;; Turn off the stupid _ as part of word thing
(modify-syntax-entry ?_ "_" python-mode-syntax-table)

;; Don't split when we run some code (doesn't work well with frames).
(py-split-windows-on-execute-off)

;; Turn off outline minor mode (never use it an it messes up keybinds).
(setq py-outline-minor-mode-p nil)


;; Setup jedi mode (python autocompletion)
;; ============================================================

;; Enable it, but only for autocompletion so we don't get all the keybind
;; junk.
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Build/test/check functions
;; ============================================================

(setq nose-command "nosetests -d --all-modules --processes=8")

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

  ;; Some things copied from python mode that were actually useful:
  (local-set-key [remap indent-for-tab-command] 'py-indent-line)
  (local-set-key [remap delete-forward-char] 'py-electric-delete)
  (local-set-key [remap delete-char] 'py-electric-delete)
  (local-set-key [remap delete-backward-char] 'py-electric-backspace)
  (local-set-key [remap newline] 'py-newline-and-indent)
  (local-set-key [remap newline-and-indent] 'py-newline-and-indent))


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
