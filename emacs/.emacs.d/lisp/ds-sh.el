
(require 'shell)
(require 'sh-script)

(add-hook 'shell-mode-hook 'set-tab)
(add-hook 'shell-mode-hook (lambda ()
                             (set 'sh-basic-offset 2)
                             (set 'sh-indentation 2)))

(require 'flycheck)
(add-hook 'shell-mode-hook #'flycheck-mode)
