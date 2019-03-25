(require 'evil)

(use-package ess
  :ensure t
  :config
  (require 'ess)
  (require 'ess-r-mode)
  (define-key ess-r-mode-map (kbd "C-c") nil)
  (define-key ess-r-mode-map (kbd "C-x") nil)
  (define-key ess-r-mode-map (kbd "<C-return>") nil)

  ;; ess-mode doesn't derive from prog mode :(
  (define-key ess-r-mode-map (kbd "M-q") #'fill-function-arguments-dwim)

  ;; ess-mode tries to do some stupid stuff with '_' and ',',
  ;; disable this.
  (define-key ess-r-mode-map (kbd "_") nil)
  (define-key ess-r-mode-map (kbd ",") nil)

  (add-hook 'ess-roxy-mode-hook
            (lambda () (define-key ess-roxy-mode-map (kbd "C-c") nil)))

  (add-hook 'ess-mode-hook
            (lambda() (ess-set-style 'C++ 'quiet)
              (validate-setq ess-arg-function-offset t)))

  ;; auto newline after '{'
  (add-hook 'ess-mode-hook
            (lambda()
              (add-to-list 'electric-layout-rules '( ?\{ .  after))
              (add-to-list 'electric-layout-rules '( ?\} .  before))
              ))

  ;; ess uses ido by default, turn this off so that I can use my normal
  ;; completion
  (validate-setq ess-use-ido nil)

  (validate-setq ess-ask-for-ess-directory nil)
  (validate-setq ess-fancy-comments nil)

  ;; Use rtags to make the tags file
  (require 'projectile)
  (add-hook 'ess-mode-hook
            (lambda()
              (make-local-variable 'projectile-tags-command)
              (validate-setq projectile-tags-command
                             "R -e 'rtags(recursive=TRUE,ofile=\"%s\")'")))

  (evil-define-operator ds/ess-evil-eval (beg end)
    :move-point nil
    (ess-eval-region beg end nil))
  (evil-define-key 'normal ess-r-mode-map (kbd "#") 'ds/ess-evil-eval)

  (add-to-list 'evil-insert-state-modes 'inferior-ess-mode)

  ;; my movement keys
  (define-key inferior-ess-r-mode-map (kbd "M-n") nil)
  (define-key inferior-ess-r-mode-map (kbd "M-i") nil)

  (define-key inferior-ess-r-mode-map (kbd "C-y") nil)

  (define-key inferior-ess-r-mode-map (kbd "<up>") #'comint-previous-input)
  (define-key inferior-ess-r-mode-map (kbd "<down>") #'comint-next-input)

  (defun ds/ess-pretty-function ()
    (add-to-list 'prettify-symbols-alist '("function" . 955)))
  (add-hook 'ess-mode-hook #'ds/ess-pretty-function)
  (add-hook 'ess-mode-hook #'prettify-symbols-mode t)

  )
