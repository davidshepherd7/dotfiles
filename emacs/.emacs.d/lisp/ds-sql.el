
(require 'use-package)

(use-package sqlup-mode
  :ensure t
  :diminish sqlup-mode
  :config
  (add-to-list 'sqlup-blacklist "user")
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "group")
  (add-to-list 'sqlup-blacklist "data")
  (add-hook 'sql-mode-hook #'sqlup-mode t))

(add-hook 'sql-mode-hook #'flycheck-mode)
