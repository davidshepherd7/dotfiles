(require 'straight)
(require 'use-package)
(require 'flycheck)
(require 'validate)



(use-package lsp-mode
  :config

  ;; Read from language servers in big chunks to improve perf
  (validate-setq read-process-output-max (* 1024 1024))

  ;; Don't turn everything on by default, most of the features are over-the-top
  (validate-setq lsp-auto-configure nil)

  ;; Shut up the file watch questions for money-srv
  (validate-setq lsp-file-watch-threshold 8000)

  ;; But do exclude some junk!
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'")

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (define-key lsp-signature-mode-map (kbd "M-n") nil)
  (define-key lsp-signature-mode-map (kbd "M-p") nil)

  ;; Python
  ;; (require 'lsp-pyls)
  ;; (validate-setq lsp-pyls-plugins-autopep8-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-flake8-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-pyflakes-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-pycodestyle-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-autopep8-enabled nil)
  ;; (validate-setq lsp-signature-auto-activate nil)

  ;; (defun ds/python-lsp ()
  ;;   (interactive)
  ;;   (lsp)
  ;;   (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  ;;   (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  ;;   (lsp-completion-mode)
  ;;   (flycheck-mode 1))
  ;; (add-hook 'python-mode-hook #'ds/python-lsp)
  )


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config

  (validate-setq lsp-pyright-log-level "info")
  )
