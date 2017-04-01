(when (eq system-type 'windows-nt)

  ;; At least emacs windows are tiling, better than default Windows wm
  (frames-only-mode 0)

  ;; Disable transparency
  (setq default-frame-alist (delete '(alpha 85 85) default-frame-alist))

  (validate-setq w32-lwindow-modifier 'super)
  (validate-setq w32-pass-lwindow-to-system nil)

  (global-set-key (kbd "s-i") #'other-window)


  )
