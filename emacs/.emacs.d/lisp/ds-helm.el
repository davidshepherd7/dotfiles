
(set 'helm-command-prefix-key "C-\\ c")

(use-package helm :ensure t :demand)


;; Use fuzzy matching with helm
(use-package flx
  :ensure t

  :config

  (defvar helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-file))
  (defadvice helm-score-candidate-for-pattern
      (around flx-score (candidate pattern) activate preactivate compile)
    (setq ad-return-value
          (or
           (car (flx-score
                 (substring-no-properties candidate)
                 (substring-no-properties pattern)
                 helm-flx-cache))
           0)))

  (defadvice helm-fuzzy-default-highlight-match
      (around flx-highlight (candidate) activate preactivate compile)
    "The default function to highlight matches in fuzzy matching.
It is meant to use with `filter-one-by-one' slot."
    (setq ad-return-value
          (let* ((pair (and (consp candidate) candidate))
                 (display (if pair (car pair) candidate))
                 (real (cdr pair)))
            (with-temp-buffer
              (insert display)
              (goto-char (point-min))
              (if (string-match-p " " helm-pattern)
                  (cl-loop with pattern = (split-string helm-pattern)
                           for p in pattern
                           do (when (search-forward (substring-no-properties p) nil t)
                                (add-text-properties
                                 (match-beginning 0) (match-end 0) '(face helm-match))))
                (cl-loop with pattern = (cdr (flx-score
                                              (substring-no-properties display)
                                              helm-pattern helm-flx-cache))
                         for index in pattern
                         do (add-text-properties
                             (1+ index) (+ 2 index) '(face helm-match))))
              (setq display (buffer-string)))
            (if real (cons display real) display))))

  (setq
   helm-buffers-fuzzy-matching t
   helm-imenu-fuzzy-match t
   helm-recentf-fuzzy-match t
   helm-locate-fuzzy-match nil
   helm-M-x-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-projectile-fuzzy-match t
   )

  (set 'helm-mode-fuzzy-match t)
  )



(use-package helm-projectile
  :ensure t)

(require 'helm)
(require 'helm-config)

(helm-mode 1)


;; Generic helm settings:
;; ============================================================

;; Use better colour for highlighting the current line
(set-face-attribute 'helm-selection nil :background "grey20")

;; Fuzzy matching settings
(setq helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t)

;; Move to end or beginning of source when reaching top or bottom of
;; source.
(set 'helm-move-to-line-cycle-in-source t)

;; Set tab to "next-item", like ido
(define-key helm-map [tab] #'helm-next-line)
(define-key helm-map [backtab] #'helm-previous-line)



(setq ido-use-virtual-buffers t)

;; increase number of buffers to rememeber
(set 'recentf-max-saved-items 1000)


(use-package helm-swoop
  :disabled t

  :config (progn
            (global-set-key (kbd "M-F") #'helm-swoop)
            (define-key helm-swoop-edit-map (kbd "C-s") #'helm-swoop--edit-complete))
  :ensure t)



(global-set-key (kbd "M-\\") #'helm-M-x)
(global-set-key (kbd "<menu>") #'helm-M-x)

(global-set-key (kbd "M-k") #'helm-mini)

(global-set-key (kbd "C-S-k") #'helm-find-files)
(global-set-key (kbd "C-M-k") (lambda () (interactive) (find-file-other-window (buffer-file-name))))

(global-set-key (kbd "C-.") #'helm-all-mark-rings)

(global-set-key (kbd "C-S-v") #'helm-show-kill-ring)



;; Bookmarks
;; ============================================================

(global-set-key (kbd "C-=") #'helm-bookmarks)

;; Set the location for bookmarks
(set 'bookmark-default-file "~/.emacs.d/bookmarks")


;; Keys inside helm
;; ============================================================

(define-key helm-map (kbd "C-<return>") #'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-RET") #'helm-maybe-exit-minibuffer)

;; navigation
(define-key helm-map (kbd "C-i") #'helm-previous-line)
(define-key helm-map (kbd "C-h") #'helm-next-line)
(define-key helm-map (kbd "C-t") #'helm-next-line)


(define-key helm-map (kbd "C-a") #'helm-mark-all)

;; Do things
(define-key helm-map (kbd "C-s") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-r") #'helm-select-action)

(define-key helm-map (kbd "C-<return>") #'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-RET") #'helm-maybe-exit-minibuffer)

(defun ds/helm-maybe-exit-minibuffer-other-window ()
  (interactive)
  ()
  )

(define-key helm-map (kbd "M-<return>") #'helm-execute-other-window)
(define-key helm-map (kbd "M-RET") #'helm-execute-other-window)



;; undef useful editing commands
(define-key helm-map (kbd "C-z") nil)
(define-key helm-map (kbd "C-v") nil)
(define-key helm-map (kbd "C-n") nil)
(define-key helm-map (kbd "C-e") nil)




;; Enter in file finding enters dirs
;; ============================================================

(cl-defun dwim-helm-find-files-navigate-forward
    (&optional (attr 'persistent-action) split-onewindow)
  "Adjust how helm-execute-persistent actions behaves, depending on context"
  (interactive)
  (if (file-directory-p (helm-get-selection))
      (helm-execute-persistent-action attr split-onewindow)
    (helm-maybe-exit-minibuffer)))


(define-key helm-map (kbd "<return>") #'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "RET") #'helm-maybe-exit-minibuffer)

(define-key helm-find-files-map (kbd "<return>") #'dwim-helm-find-files-navigate-forward)
(define-key helm-find-files-map (kbd "RET") #'dwim-helm-find-files-navigate-forward)
(define-key helm-find-files-map (kbd "C-<return>") #'helm-maybe-exit-minibuffer)
(define-key helm-find-files-map (kbd "C-RET") #'helm-maybe-exit-minibuffer)

(define-key helm-read-file-map (kbd "<return>") #'dwim-helm-find-files-navigate-forward)
(define-key helm-read-file-map (kbd "RET") #'dwim-helm-find-files-navigate-forward)
(define-key helm-read-file-map (kbd "C-<return>") #'helm-maybe-exit-minibuffer)
(define-key helm-read-file-map (kbd "C-RET") #'helm-maybe-exit-minibuffer)

(require 'helm-help)
