
(require 'shell)
(require 'sh-script)
(require 'validate)

(add-hook 'sh-mode-hook 'set-tab)

(require 'flycheck)
(add-hook 'sh-mode-hook #'flycheck-mode)

;; My shellcheck doesn't support this
(validate-setq flycheck-shellcheck-follow-sources nil)

(add-hook 'sh-mode-hook #'sh-electric-here-document-mode)
