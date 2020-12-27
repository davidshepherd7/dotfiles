
(require 'use-package)
(require 'sql)

(use-package sqlup-mode
  :diminish sqlup-mode
  :config
  (add-to-list 'sqlup-blacklist "user")
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "group")
  (add-to-list 'sqlup-blacklist "data")
  (add-to-list 'sqlup-blacklist "version")
  (add-to-list 'sqlup-blacklist "type")
  (add-hook 'sql-mode-hook #'sqlup-mode t))

(add-hook 'sql-mode-hook #'flycheck-mode)

(defun ds/set-product-postgres ()
  (sql-set-product "postgres"))
(add-hook 'sql-mode-hook #'ds/set-product-postgres)


(use-package sql-indent
  :config
  (add-hook 'sql-mode-hook #'sqlind-minor-mode)
  (setq-default sqlind-indentation-offsets-alist
                `((select-clause +)
                  (insert-clause +)
                  (delete-clause +)
                  (update-clause +)
                  (statement-continuation +)
                  ,@sqlind-default-indentation-offsets-alist))
  (setq-default sqlind-basic-offset 4)
  )
