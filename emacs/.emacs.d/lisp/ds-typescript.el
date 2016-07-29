
(use-package typescript-mode
  :ensure t

  :config

  (require 'ds-js)
  (require 'typescript-mode)

  (add-hook 'typescript-mode-hook #'electric-operator-mode)
  (add-hook 'typescript-mode-hook #'aggressive-indent-mode)

  (require 'align)
  (add-to-list 'align-sq-string-modes 'typescript-mode)

  (add-hook 'typescript-mode-hook #'ds/pretty-function)
  (add-hook 'typescript-mode-hook #'prettify-symbols-mode t)

  (js-extra-keywords 'typescript-mode)

  (define-key typescript-mode-map (kbd "C-\\ o") #'ds/switch-to-html)
  (define-key typescript-mode-map (kbd "C-\\ n") #'ds/js-switch-to-test)

  (defun boron-typings-index ()
    (f-join (projectile-project-root) "boron" "web_applications" "typings" "index.d.ts"))

  (flycheck-define-checker typescript-tsc
    "A TypeScript syntax checker using tsc command."
    :command ("tsc" "--out" "/dev/null" (eval (boron-typings-index)) source)
    :error-patterns
    ((error line-start (file-name) "(" line "," column "): error " (message) line-end))
    :next-checker (typescript-tslint)
    :mode typescript-mode)

  (add-to-list 'flycheck-checkers 'typescript-tsc)

  (add-hook 'typescript-mode-hook #'flycheck-mode)

  ;; Parse typescript compiler output
  (require 'compile)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(typescript "\\(.*\\)(\\([0-9]*\\),\\([0-9]*\\)): " 1 2))
  (add-to-list 'compilation-error-regexp-alist 'typescript)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "at [^( ]+ (\\(.*?\\):\\([0-9]*\\):\\([0-9]*\\))" 1 2))
  (add-to-list 'compilation-error-regexp-alist 'node)

  ;; Fuck this failing auto indent shit
  (-each (list (kbd ",") (kbd "(") (kbd ")") (kbd ":") (kbd ";"))
    (lambda (key) (define-key typescript-mode-map key nil)))

  )
