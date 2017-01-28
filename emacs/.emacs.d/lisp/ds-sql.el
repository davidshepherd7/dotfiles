
(require 'use-package)
(require 'sql)

(use-package sqlup-mode
  :ensure t
  :diminish sqlup-mode
  :config
  (add-to-list 'sqlup-blacklist "user")
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "group")
  (add-to-list 'sqlup-blacklist "data")
  (add-hook 'sql-mode-hook #'sqlup-mode t))

(use-package sql-indent
  :load-path "~/.emacs.d/sql-indent/"
  :config
  (add-hook 'sql-mode-hook #'sql-indent-mode))

(add-hook 'sql-mode-hook #'flycheck-mode)

(defun ds/set-product-postgres ()
  (sql-set-product "postgres"))
(add-hook 'sql-mode-hook #'ds/set-product-postgres)

