
(require 'use-package)

(use-package sqlup-mode
  :ensure t
  :diminish sqlup-mode
  :config
  (add-hook 'sql-mode-hook #'sqlup-mode t))

(add-hook 'sql-mode-hook #'flycheck-mode)
