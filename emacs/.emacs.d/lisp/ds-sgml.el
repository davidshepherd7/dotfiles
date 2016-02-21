
(require 'sgml-mode)

(defun insert-br-tag ()
  (interactive)
  (insert "<br>"))
(define-key html-mode-map (kbd "M-RET") #'insert-br-tag)

(require 'aggressive-indent)
(add-hook 'sgml-mode-hook #'aggressive-indent-mode)

(defun ds/switch-to-js ()
  (interactive)
  (ds/switch-to-related ".js"))
(define-key html-mode-map (kbd "C-\\ o") #'ds/switch-to-js)
