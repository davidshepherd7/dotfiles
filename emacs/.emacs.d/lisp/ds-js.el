
(defun ds/disable-electric-semicolon ()
  (interactive)
  (add-to-list 'electric-layout-rules (cons ?\; nil)))
(add-hook 'js-mode-hook #'ds/disable-electric-semicolon)

(add-hook 'js-mode-hook #'electric-operator-mode)


(add-to-list 'grep-find-ignored-files "*.min.js")
(add-to-list 'grep-find-ignored-files "*.min.js.map")

;; Don't really know what these are but they show up in the biosite repo
(add-to-list 'grep-find-ignored-files "*.bmml")
