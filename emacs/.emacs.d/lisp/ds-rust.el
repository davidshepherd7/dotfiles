(require 'package)
(require 'use-package)

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'electric-operator-mode)
  )
