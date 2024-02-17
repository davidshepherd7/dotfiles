;; Scheme
;;=================================================================

(require 'scheme)

;; Set scheme interpreter name
(setq scheme-program-name "scheme")

;; Send whole buffer to scheme evaluator
(defun scheme-send-buffer()
  (interactive)
  (save-buffer 1)
  (scheme-send-region (point-min) (point-max)))

(defun mechanics ()
  (interactive)
  (run-scheme "mechanics"))


;; (defun new-scheme-prompt-from-buffer()
;;   (interactive)
;;   (save-buffer 1)
;;   ;; send EOF to scheme ??
;;   (kill-buffer "*scheme*")
;;   (setq scheme-program-name (format "mit-scheme --load %s" (buffer-name)))
;;   (run-scheme)
;;   )

;; make f5 run the current buffer
(add-hook 'scheme-mode-hook #'(lambda() (setq-default require-final-newline nil)))

;; Display .rkt (racket) files using scheme mode
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(defun ds/scheme-pretty-lambda ()
  (add-to-list 'prettify-symbols-alist '("lambda" . 955))
  (prettify-symbols-mode t))

(add-hook 'scheme-mode-hook #'ds/scheme-pretty-lambda t)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)


(defun ds/prev-define ()
  (interactive)
  (re-search-backward "(define\\s-*\(\\s-*\\(\\S-+\\)\\s-")
  (match-string 1))


(provide 'ds/scheme)
