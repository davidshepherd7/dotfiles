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
    (define-key org-mode-map (kbd "TAB") nil)

    (define-key org-mode-map  (kbd "C-t") #'org-todo)
    (define-key org-mode-map  (kbd "C-.") #'org-time-stamp-inactive)


    ;; Pretty indents
    (add-hook 'org-mode-hook #'org-indent-mode)

    ;; TODO disable whitespace cleanup on curent line

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

    (define-key org-mode-map (kbd "M-s") #'org-schedule)

    (validate-setq org-agenda-files '("~/.tt"))

    ))

;; (defun aggressive-fill-paragraph ()
;;   (interactive)
;;   (unless (dont-fill-if)))

;; (defun set-auto-fill-hook()
;;   (interactive)
;;   ;; (add-hook 'after-change-functions #'fill-paragraph nil t)
;;   (add-hook 'post-command-hook #'fill-paragraph nil t))

;; (add-hook 'org-mode-hook 'set-auto-fill-hook)

(defun ds/org-to-quip (text)
  (--> text
       (replace-regexp-in-string "^\\*" "" it)
       (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1    *" it)
       (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1    *" it)
       (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1    *" it)
       (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1    *" it)
       ))


(defun ds/copy-as-quip (beg end)
  (interactive "r")
  (kill-new (ds/org-to-quip (buffer-substring-no-properties beg end))))

(defun ds/org-goto-first-todo ()
  "Jump to the first TODO entry in the buffer."
  (interactive)
  (goto-char (point-min))
  (outline-next-heading)
  (while (not (org-entry-is-todo-p))
    (forward-line 1)))

(defun ds/show-wave-todo-list ()
  ;; If the file is already open in another frame then just opening it in the
  ;; normal way again doesn't display it in the new frame. So we use find-file
  ;; manually instead.
  (find-file "~/.tt/wave-todo.org")
  (ds/org-goto-first-todo)
  (recenter 3))
