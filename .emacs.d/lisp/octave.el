(autoload 'octave-mode "octave-mod" nil t)

;; use octave for .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;; fix tab for indent from yasnippet
(add-hook 'octave-mode-hook 'set-tab)


(defun octave-keys ()
  (local-set-key (kbd "C-c") 'nil))

;; unbind C-c
(add-hook 'octave-mode-hook 'octave-keys)
