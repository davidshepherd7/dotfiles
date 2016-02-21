
(require 'use-package)

(use-package sqlup-mode
  :ensure t
  :config
  (add-hook 'sql-mode-hook #'sqlup-mode))
