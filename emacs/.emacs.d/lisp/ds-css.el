
(require 'css-mode)

(add-hook 'css-mode-hook #'set-tab t)
(add-hook 'css-mode-hook #'electric-operator-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
