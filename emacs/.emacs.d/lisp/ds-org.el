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
    (add-hook 'org-mode-hook 'org-indent-mode)


    (setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                       (vm-imap . vm-visit-imap-folder-other-frame)
                                       (gnus . org-gnus-no-new-news)
                                       (file . find-file-other-frame)
                                       (wl . wl-other-frame)))))
  )

;; (defun aggressive-fill-paragraph ()
;;   (interactive)
;;   (unless (dont-fill-if)))

;; (defun set-auto-fill-hook()
;;   (interactive)
;;   ;; (add-hook 'after-change-functions #'fill-paragraph nil t)
;;   (add-hook 'post-command-hook #'fill-paragraph nil t))

;; (add-hook 'org-mode-hook 'set-auto-fill-hook)
