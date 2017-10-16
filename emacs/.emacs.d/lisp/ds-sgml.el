
(require 'sgml-mode)

(use-package sgml-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook #'set-tab))

(defun insert-br-tag ()
  (interactive)
  (insert "<br>"))
(define-key html-mode-map (kbd "M-RET") #'insert-br-tag)

(require 'aggressive-indent)
(add-hook 'sgml-mode-hook #'aggressive-indent-mode)

(defcustom ds/sgml-switch-extension ".js"
  "File extension to switch to")

(defun ds/switch-to-js ()
  (interactive)
  (ds/switch-to-related ds/sgml-switch-extension))
(define-key html-mode-map (kbd "C-\\ o") #'ds/switch-to-js)

(defun ds/html-switch-to-test ()
  (interactive)
  (ds/switch-to-related (concat ".spec" ds/sgml-switch-extension)))
(define-key html-mode-map (kbd "C-\\ n") #'ds/html-switch-to-test)


(define-key sgml-mode-map (kbd "C-M-n") #'sgml-skip-tag-forward)
(define-key sgml-mode-map (kbd "C-M-e") #'sgml-skip-tag-backward)

(define-key sgml-mode-map (kbd "<") nil)

(require 'flycheck)
(add-hook 'html-mode-hook #'flycheck-mode)
(add-hook 'css-mode-hook #'flycheck-mode)

(add-to-list 'auto-mode-alist (cons "\\.wxs" #'xml-mode))
(add-to-list 'auto-mode-alist (cons "\\.mustache$" #'html-mode))
