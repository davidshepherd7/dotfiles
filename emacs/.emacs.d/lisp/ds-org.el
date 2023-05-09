(require 'validate)
(require 'dash)
(require 'use-package)


(use-package org
  :config
  (progn

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

    (define-key org-mode-map (kbd "M-s") #'org-schedule)

    (validate-setq org-agenda-files '("~/Dropbox/org"))

    (setq org-todo-keywords '((sequence "TODO" "CHASE" "|" "DONE")))

    (validate-setq org-todo-keyword-faces
                   (list (cons "CHASE" "powder blue")))

    (defun ds/org-sort-buffer ()
      (interactive)
      (save-excursion
        (set-mark (point-min))
        (goto-char (point-max))
        (org-sort-entries nil ?O))))

  (set-face-attribute 'org-level-4 nil :foreground "SkyBlue")
  (set-face-attribute 'org-level-5 nil :foreground "Grey86")
  (set-face-attribute 'org-level-6 nil :foreground "Grey86")
  (set-face-attribute 'org-level-7 nil :foreground "Grey86")
  (set-face-attribute 'org-level-8 nil :foreground "Grey86")

  ;; Place point just after *** when going to the start of a heading
  (validate-setq org-special-ctrl-a/e t)
  )

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
    (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1  *" it)
    (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1  *" it)
    (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1  *" it)
    (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1  *" it)
    (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1  *" it)
    (replace-regexp-in-string "^\\(\\s-*\\)\\*\\*" "\\1  *" it)
    ))


(defun ds/copy-as-quip (beg end)
  (interactive "r")
  (kill-new (ds/org-to-quip (buffer-substring-no-properties beg end))))

(defun ds/org-goto-first-todo ()
  "Jump to the first TODO entry in the buffer."
  (interactive)
  (goto-char (point-min))
  (outline-next-heading)
  (while (and (not (org-entry-is-todo-p))
              (not (equal (point-at-eol) (point-max))))
    (forward-line 1)))

(defun ds/show-wave-todo-list ()
  ;; If the file is already open in another frame then just opening it in the
  ;; normal way again doesn't display it in the new frame. So we use find-file
  ;; manually instead.
  (find-file "~/Dropbox/org/wave-todo.org")
  (ds/org-goto-first-todo)
  (recenter 3))


(defun ds/show-personal-todo-list ()
  ;; If the file is already open in another frame then just opening it in the
  ;; normal way again doesn't display it in the new frame. So we use find-file
  ;; manually instead.
  (find-file "~/Dropbox/org/todo.org")
  (ds/org-goto-first-todo)
  (recenter 3))
