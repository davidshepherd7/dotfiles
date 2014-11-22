
;; Load org mode when opening .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; keys
(defun org-keys ()
  "Clear lots of org mode keys that mess with my keybinds"
  (interactive)
  (local-set-key (kbd "C-c") 'nil)
  (local-set-key (kbd "C-y") 'nil)
  (local-set-key (kbd "C-e") 'nil)
  (local-set-key (kbd "C-t") 'org-todo))

(add-hook 'org-mode-hook 'org-keys)

;; Pretty indents
(add-hook 'org-mode-hook 'org-indent-mode)

(set 'org-agenda-files '("~/Dropbox/org/thesis_plan.org"))

(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file-other-frame)
                                   (wl . wl-other-frame))))

;; (defun aggressive-fill-paragraph ()
;;   (interactive)
;;   (unless (dont-fill-if)))

;; (defun set-auto-fill-hook()
;;   (interactive)
;;   ;; (add-hook 'after-change-functions #'fill-paragraph nil t)
;;   (add-hook 'post-command-hook #'fill-paragraph nil t))

;; (add-hook 'org-mode-hook 'set-auto-fill-hook)
