(require 'package)
(require 'use-package)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'electric-operator-mode)
  (add-hook 'rust-mode-hook #'lsp-mode))

