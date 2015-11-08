
(defun ds/disable-electric-semicolon ()
  (interactive)
  (add-to-list 'electric-layout-rules (cons ?\; nil)))
(add-hook 'js-mode-hook #'ds/disable-electric-semicolon)

(add-hook 'js-mode-hook #'electric-operator-mode)

(require 'grep)
(add-to-list 'grep-find-ignored-files "*.min.js")
(add-to-list 'grep-find-ignored-files "*.min.js.map")

;; Don't really know what these are but they show up in the biosite repo
(add-to-list 'grep-find-ignored-files "*.bmml")

(add-hook 'js-mode-hook #'aggressive-indent-mode)

;; Show function as lambda. Append prettify-symbols-mode so that symbol for
;; function is added before the mode is enabled.
(defun ds/pretty-function ()
  (add-to-list 'prettify-symbols-alist '("function" . 955)))
(add-hook 'js-mode-hook #'ds/pretty-function)
(add-hook 'js-mode-hook #'prettify-symbols-mode t)
