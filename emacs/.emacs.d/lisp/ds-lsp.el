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

  ;; Don't show documentation unless I ask for it
  (validate-setq lsp-signature-auto-activate nil)

  ;; Don't truncate logs
  (validate-setq lsp-log-max t)

  ;; But do exclude some junk!
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]support/alembic_data\\'")

  ;; Looking up references takes a while
  (validate-setq lsp-response-timeout 30)

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (define-key lsp-signature-mode-map (kbd "M-n") nil)
  (define-key lsp-signature-mode-map (kbd "M-p") nil)

  (define-key lsp-mode-map (kbd "C-<f8>") #'lsp-find-references)


  ;; Don't init in like every single code directory on my laptop, WTF?
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

  ;; Python
  ;; (require 'lsp-pyls)
  ;; (validate-setq lsp-pyls-plugins-autopep8-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-flake8-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-pyflakes-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-pycodestyle-enabled nil)
  ;; (validate-setq lsp-pyls-plugins-autopep8-enabled nil)

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

  (validate-setq lsp-pyright-log-level
                 ;;"trace"
                 "warning"
                 )
  )
