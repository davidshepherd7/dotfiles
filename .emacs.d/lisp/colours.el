;; ========= Set colours ==========
;; from http://alexpogosyan.com/color-theme-creator/
(defun DarkStrongColours ()
  (interactive)
  (color-theme-install
   '(DarkStrongColours
     ((background-color . "#000000")
      (background-mode . light)
      (border-color . "#122245")
      (cursor-color . "#fff394")
      (foreground-color . "#ffffff")
      (mouse-color . "black"))
     (fringe ((t (:background "#000000"))))
     (mode-line ((t (:foreground "#ffffff" :background "#000000"))))
     (region ((t (:background "#105b20"))))
     (font-lock-builtin-face ((t (:foreground "#007bff"))))
     (font-lock-comment-face ((t (:foreground "#858585"))))
     (font-lock-function-name-face ((t (:foreground "#ffe400"))))
     (font-lock-keyword-face ((t (:foreground "#00f5b6"))))
     (font-lock-string-face ((t (:foreground "#c346b5"))))
     (font-lock-type-face ((t (:foreground"#83ff0a"))))
     (font-lock-variable-name-face ((t (:foreground "#ff6b05"))))
     (minibuffer-prompt ((t (:foreground "#ffffff" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(provide 'DarkStrongColours)

(use-package color-theme
  :ensure t
  :config (progn
            (color-theme-initialize)
            (DarkStrongColours)))
