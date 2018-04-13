(require 'use-package)
(require 'company)
(require 'evil)

(defun ds/slime-compile-load-and-raise ()
  (interactive)
  (slime-compile-and-load-file)
  (slime-switch-to-output-buffer))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))

  (define-key slime-mode-map (kbd "<f5>") #'ds/slime-compile-load-and-raise)

  (define-key slime-mode-map (kbd "M-n") nil)
  (define-key slime-mode-map (kbd "M-i") nil)
  (define-key slime-mode-map (kbd "M-¬") #'slime-previous-note)
  (define-key slime-mode-map (kbd "M-`") #'slime-next-note)

  (evil-define-operator ds/slime-evil-eval (beg end)
    :move-point nil
    (slime-eval-region beg end))
  (evil-define-key 'normal slime-mode-map (kbd "#") 'ds/slime-evil-eval)

  (require 'slime-repl)

  (define-key slime-repl-mode-map (kbd "<up>") #'slime-repl-previous-input)
  (define-key slime-repl-mode-map (kbd "<down>") #'slime-repl-next-input)
  (define-key slime-repl-mode-map (kbd "SPC") #'slime-space)

  ;; my movement keys
  (define-key slime-repl-mode-map (kbd "M-n") nil)
  (define-key slime-repl-mode-map (kbd "M-i") nil))

(use-package slime-company
  :ensure t
  :config

  (defun ds/slime-company-setup ()
    (interactive)
    (make-variable-buffer-local company-backends)
    (push 'company-slime company-backends))

  (add-hook 'slime-mode-hook #'ds/slime-company-setup)
  (define-key slime-repl-mode-map (kbd "<tab>") #'company-complete-common-or-cycle)

  (add-hook 'common-lisp-mode-hook #'ds/slime-company-setup))


(define-key lisp-mode-map (kbd "M-n") nil)
(define-key lisp-mode-map (kbd "M-i") nil)

(add-hook 'common-lisp-mode-hook #'prettify-symbols-mode)
