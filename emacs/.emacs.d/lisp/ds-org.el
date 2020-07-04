(use-package org
  :ensure t
  :config
  (progn
    (require 'org)

    ;; Load org mode when opening .org files
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

    ;; keys
    (define-key org-mode-map  (kbd "C-y") nil)
    (define-key org-mode-map  (kbd "C-e") nil)
    (define-key org-mode-map  (kbd "C-'") nil)
    (define-key org-mode-map  (kbd "C-a") nil)
    (define-key org-mode-map (kbd "M-e") nil)

    (define-key org-mode-map  (kbd "C-t") #'org-todo)
    (define-key org-mode-map  (kbd "C-.") #'org-time-stamp-inactive)


    ;; Pretty indents
    (add-hook 'org-mode-hook #'org-indent-mode)


    (setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                       (vm-imap . vm-visit-imap-folder-other-frame)
                                       (gnus . org-gnus-no-new-news)
                                       (file . find-file-other-frame)
                                       (wl . wl-other-frame))))

    (require 'evil)

    (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
    (define-key org-mode-map (kbd "<tab>") #'org-cycle)

    (evil-define-key 'normal org-mode-map (kbd "C-i") #'org-metaup)
    (evil-define-key 'normal org-mode-map (kbd "C-h") #'org-metadown)
    (evil-define-key 'normal org-mode-map (kbd "C-n") #'org-metaleft)
    (evil-define-key 'normal org-mode-map (kbd "C-e") #'org-metaright)

    (evil-define-key 'motion org-mode-map (kbd "y") #'org-backward-element)
    (evil-define-key 'motion org-mode-map (kbd "u") #'org-forward-element)
    (evil-define-key 'motion org-mode-map (kbd "j") #'outline-up-heading)

    (defun ds/org-insert-subheading-after-current ()
      (interactive)
      (org-insert-heading-after-current)
      (org-demote))

    (evil-define-key 'normal org-mode-map (kbd "<C-return>")
      (lambda () (interactive) (ds/org-insert-subheading-after-current) (evil-insert nil)))
    (define-key org-mode-map (kbd "C-<return>") #'ds/org-insert-subheading-after-current)
    (evil-define-key 'normal org-mode-map (kbd "M-<return>")
      (lambda () (interactive) (org-insert-heading-after-current) (evil-insert nil)))
    (define-key org-mode-map (kbd "M-<return>") #'org-insert-heading-after-current)

    ))

;; (defun aggressive-fill-paragraph ()
;;   (interactive)
;;   (unless (dont-fill-if)))

;; (defun set-auto-fill-hook()
;;   (interactive)
;;   ;; (add-hook 'after-change-functions #'fill-paragraph nil t)
;;   (add-hook 'post-command-hook #'fill-paragraph nil t))

;; (add-hook 'org-mode-hook 'set-auto-fill-hook)
