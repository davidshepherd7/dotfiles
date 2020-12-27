
;; (use-package matlab
;;   :ensure matlab-mode
;;   :config (progn

;;             (define-key matlab-mode-map (kbd "C-c") 'nil)

;;             (add-hook 'matlab-mode-hook
;;                       '(lambda()
;;                          ;; fix comment keybind from their stupid one
;;                          (local-set-key (kbd "M-;") 'comment-dwim)))

;;             ;; fix tab for indent from yasnippet
;;             (add-hook 'matlab-mode-hook 'set-tab)

;;             ;; ;; Set .m file to use matlab-mode
;;             ;; (setq auto-mode-alist
;;             ;;       (cons '("\\.m$" . matlab-mode) auto-mode-alist))
;;             ))
