
;; Automatically use python mode from "python-mode.el"
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Turn "lambda " into a lambda symbol
(require 'lambda-mode)
(add-hook 'python-mode-hook 'lambda-mode 1)
(setq lambda-symbol (string #x1d77a))
(setq lambda-regex "lambda ")


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
  (local-set-key (kbd "C-`") 'next-error)
  (local-set-key (kbd "C-¬") 'previous-error)
  (local-set-key (kbd "<f6>") 'renose)
  (local-set-key (kbd "#") 'self-insert-command))


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
