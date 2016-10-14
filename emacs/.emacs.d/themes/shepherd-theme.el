;; Code is based on https://github.com/owainlewis/emacs-color-themes,
;; colours possibly based on something else but selected so long ago that I
;; don't remember.

(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

;; I recommend:
;; * using smart mode line to colour the modeline
;; * disabling the scroll bar


(deftheme shepherd "A dark theme with strong colours and transparency")

;; make transparent
(add-to-list 'default-frame-alist '(alpha 85 85))

(custom-theme-set-faces
 'shepherd

 ;; ----------------- Frame stuff --------------------
 `(default ((t (:background "black" :foreground "white"))))
 `(cursor  ((t (:background "#fff394"))))
 `(region ((t (:background "#2A4D6E"))))

 ;; edges
 `(fringe ((t (:background "grey2"))))
 `(linum ((t (:background "grey2" :foreground "grey20"))))
 `(menu ((t (:background "grey2" :foreground "white"))))

 ;; un-editable text in minibuffer
 `(minibuffer-prompt ((default (:foreground "grey"))))

 ;; ---------------- Code Highlighting ---------------
 `(font-lock-builtin-face ((t (:foreground "#007bff"))))
 `(font-lock-constant-face ((t (:foreground "firebrick"))))
 `(font-lock-comment-face ((t (:foreground "#858585"))))
 `(font-lock-function-name-face ((t (:foreground "#ffe400"))))
 `(font-lock-keyword-face ((t (:foreground "#00f5b6"))))
 `(font-lock-string-face ((t (:foreground "#c346b5"))))
 `(font-lock-variable-name-face ((t (:foreground "#ff6b05"))))
 `(font-lock-type-face ((t (:foreground "#83ff0a"))))
 `(font-lock-warning-face ((t (:foreground "Red")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'shepherd)

;; Local Variables:
;; no-byte-compile: t
;; End:
