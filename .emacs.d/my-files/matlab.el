(add-hook 'matlab-mode-hook
	  '(lambda()
	     ;; fix comment keybind from their stupid one
	     (local-set-key (kbd "M-;") 'comment-dwim)))

;; Set .m file to use matlab-mode
(setq auto-mode-alist
      (cons '("\\.m$" . matlab-mode) auto-mode-alist))