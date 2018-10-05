(require 'use-package)
(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook #'flycheck-mode)
  )
