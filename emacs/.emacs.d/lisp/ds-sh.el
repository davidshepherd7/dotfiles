
(require 'shell)
(require 'sh-script)

(add-hook 'sh-mode-hook 'set-tab)

(require 'flycheck)
(add-hook 'sh-mode-hook #'flycheck-mode)

(add-hook 'sh-mode-hook #'sh-electric-here-document-mode)
