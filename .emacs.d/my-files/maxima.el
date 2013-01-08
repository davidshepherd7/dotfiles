(add-to-list 'auto-mode-alist '("\\.ma[cx]?$" . maxima-mode))

;;; add to load-path the directory where imaxima.el is installed.
;;; If you change the install directory of imaxima, the first argument of push must be
;;; changed accordingly.
(push "usr/share/emacs/site-lisp/maxima" load-path)

;;; add to load-path the directory where maxima.el is installed.
;;; If you change the install directory of maxima, the first argument of push must be
;;; changed accordingly.
(push "/usr/share/maxima/5.26.0/emacs" load-path)

;;; add autoload of imaxima and maxima.
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Frontend for maxima" t)
;;; add autoload of imath.
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)

;;; Make the line effective if you want to use maxima mode with imaxima.
;; (setq imaxima-use-maxima-mode-flag nil)

(defun my-maxima-send-buffer()
  (interactive)
  (save-buffer 1)
  (maxima-string "kill(all)")	  ;kill previous variables etc.
  (maxima-send-buffer)
  )


(add-hook 'maxima-mode-hook
	  '(lambda()
	     (local-unset-key (kbd "M-;"))
	     (local-set-key (kbd "<f5>") 'my-maxima-send-buffer)
	     ))