
;; Load org mode when opening .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; keys
(defun org-keys ()
  "Clear lots of org mode keys that mess with my keybinds"
  (interactive)
  (local-set-key (kbd "C-c") 'nil)
  (local-set-key (kbd "C-y") 'nil))
(add-hook 'org-mode-hook 'org-keys)

;; Pretty indents
(add-hook 'org-mode-hook 'org-indent-mode)

(set 'org-agenda-files '("~/Dropbox/org/thesis_plan.org"))

 (setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                    (vm-imap . vm-visit-imap-folder-other-frame)
                                    (gnus . org-gnus-no-new-news)
                                    (file . find-file-other-frame)
                                    (wl . wl-other-frame))))
