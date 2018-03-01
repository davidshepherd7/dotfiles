(require 'package)
(require 'use-package)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'electric-operator-mode))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)

  (defun ds/set-company-completion-racer-only ()
    "Make company completions use racer first"
    (setq-local completion-at-point-functions '(racer-complete-at-point))
    (setq-local company-backends (cons '(company-capf) company-backends))

    (add-hook 'racer-mode-hook #'ds/set-company-completion-racer-only)))
