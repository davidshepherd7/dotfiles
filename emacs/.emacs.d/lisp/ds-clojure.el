

(use-package cider
  :ensure t
  :config

  (-each (list (cons (kbd "C-h") nil)
               (cons (kbd "C-i") nil)
               (cons (kbd "M-n") nil))
    (lambda (pair) (define-key cider-mode-map (car pair) (cdr pair))))

  (set 'cider-repl-display-help-banner nil))

(use-package clojure-mode
  :ensure t
  :config

  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'page-break-lines-mode)
  (add-hook 'clojure-mode-hook (lambda () (helm-dash-activate-docset "Clojure")))
  )
