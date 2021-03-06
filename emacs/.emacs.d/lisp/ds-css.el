
(require 'css-mode)

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook #'rainbow-mode)
  (add-hook 'scss-mode-hook #'rainbow-mode)
  )

(font-lock-add-keywords
 'css-mode
 '(("\\(\\s-\\|^\\)\\(\\.\\)\\S-" 2 font-lock-keyword-face)))

(add-hook 'css-mode-hook #'set-tab t)
(add-hook 'css-mode-hook #'electric-operator-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
