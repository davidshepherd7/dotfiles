
;; Load librarys
;;(require 'pymacs)
(require 'python-mode)
(require 'ipython)

;; Auto python-mode on .py files
;;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

 ;; (require 'lambda-mode)
;; (add-hook 'python-mode-hook #'lambda-mode 1)
;; (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

;; ;; pylookup?

;; ;; anything for completion

;; keybinds
(add-hook 'py-mode-hook
          '(lambda ()
             (local-set-key (kbd "<f5>") '(lambda ()
                                            (save-some-buffers)
                                            (py-execute-buffer)))
             ))

;; Make absolutely sure running commands in the shell doesn't do stupid things
;; to my window layout.
(add-hook 'py-mode-hook
          '(lambda ()
             (py-shell-switch-buffers-on-execute-off)
             (py-split-windows-on-execute-off)))

;; ;; (defun my-py-execute-buffer ()
;; ;;   "Execute without switching buffers."
;; ;;   (interactive)
;; ;;   (with-current-buffer (current-buffer)
;; ;;     (py-execute-buffer)))

;; ;; Autocomplete
;; (defvar ac-source-python
;;   '((candidates .
;; 		(lambda ()
;; 		  (mapcar '(lambda (completion)
;; 			     (first (last (split-string completion "\\." t))))
;; 			  (python-symbol-completions (python-partial-symbol)))))))

;; (add-hook 'python-mode-hook
;; 	  '(lambda() (setq ac-sources '(ac-source-python))))