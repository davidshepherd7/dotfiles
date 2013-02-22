

;; Automatically use python mode from "python-mode.el"
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Turn "lambda " into a lambda symbol
(require 'lambda-mode)
(add-hook 'python-mode-hook 'lambda-mode 1)
(setq lambda-symbol (string #x1d77a))
(setq lambda-regex "lambda ")

;; Turn off the stupid _ as part of word thing
(modify-syntax-entry ?_ "_" python-mode-syntax-table)

;; Don't split when we run some code (doesn't work well with frames).
(py-split-windows-on-execute-off)

;; Turn off outline minor mode (never use it an it messes up keybinds).
(setq py-outline-minor-mode-p nil)


;; Build/test/check functions
;; ============================================================

(setq nose-command "nosetests -d --all-modules")

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



(defun run-this-file ()
  "Recompile if possible, otherwise run current buffer. Will be
weird with c++ compiles..."
  (interactive)
  (let* ((py-buffer (buffer-name)))
    (compile (concat "python " py-buffer) t)))


;; Mode hooks
;; ============================================================
(add-hook 'python-mode-hook 'add-py-save-hooks)
(add-hook 'python-mode-hook 'py-keybinds)


(defun add-py-save-hooks ()
  (add-hook 'before-save-hook 'py-whitespace-cleanup))

(defun py-whitespace-cleanup ()
    (interactive)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max)))

(defun py-keybinds ()
  (use-local-map '()) ;; disable all keys
  (local-set-key (kbd "C-`") 'next-error)
  (local-set-key (kbd "C-¬") 'previous-error)
  (local-set-key (kbd "<f6>") 'renose)
  (local-set-key (kbd "<f5>") 'run-this-file)

  ;; Some things copied from python mode that were actually useful:
  (local-set-key (kbd "C-\\ <") 'py-shift-left)
  (local-set-key (kbd "C-\\ >") 'py-shift-right)

  (local-set-key [remap indent-for-tab-command] 'py-indent-line)
  (local-set-key [remap delete-forward-char] 'py-electric-delete)
  (local-set-key [remap delete-char] 'py-electric-delete)
  (local-set-key [remap delete-backward-char] 'py-electric-backspace)
  (local-set-key [remap newline] 'py-newline-and-indent)
  (local-set-key [remap newline-and-indent] 'py-newline-and-indent)
  )



;; Crashy...
;; ;; Rope
;; ;; ============================================================
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")


;; Old
;; ============================================================


;; ;; Load librarys
;; ;;(require 'pymacs)
;; (require 'python-mode)
;; (require 'ipython)

;; Auto python-mode on .py files
;;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))



;; ;; pylookup?

;; ;; anything for completion

;; ;; keybinds
;; (add-hook 'py-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "<f5>") '(lambda ()
;;                                             (save-some-buffers)
;;                                             (py-execute-buffer)))
;;              ))

;; ;; Make absolutely sure running commands in the shell doesn't do stupid things
;; ;; to my window layout.
;; (add-hook 'py-mode-hook
;;           '(lambda ()
;;              (py-shell-switch-buffers-on-execute-off)
;;              (py-split-windows-on-execute-off)))

;; ;; (defun my-py-execute-buffer ()
;; ;;   "Execute without switching buffers."
;; ;;   (interactive)
;; ;;   (with-current-buffer (current-buffer)
;; ;;     (py-execute-buffer)))

;; ;; Autocomplete
;; (defvar ac-source-python
;;   '((candidates .
;;              (lambda ()
;;                (mapcar '(lambda (completion)
;;                           (first (last (split-string completion "\\." t))))
;;                        (python-symbol-completions (python-partial-symbol)))))))

;; (add-hook 'python-mode-hook
;;        '(lambda() (setq ac-sources '(ac-source-python))))
