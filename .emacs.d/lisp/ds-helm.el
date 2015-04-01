
(set 'helm-command-prefix-key "C-\\ c")

(use-package helm :ensure t :demand)
(use-package helm-projectile
  :disabled t
  :ensure t)

(require 'helm)
(require 'helm-config)

;; (helm-mode 1)


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


;; This is set in ds-ido at the moment

;; (setq ido-use-virtual-buffers t)

;; ;; increase number of buffers to rememeber
;; (set 'recentf-max-saved-items 1000)


(use-package helm-swoop
  :disabled t

  :config (progn
            (global-set-key (kbd "M-F") #'helm-swoop)
            (define-key helm-swoop-edit-map (kbd "C-s") #'helm-swoop--edit-complete))
  :ensure t)



;; (global-set-key (kbd "M-\\") #'helm-M-x)

;; ;; Use helm-buffers-list over helm-mini because with this setting it uses
;; ;; "virtual buffers" (allows you to switch to buffers that you closed).
;; (global-set-key (kbd "M-k") #'helm-buffers-list)

;; (global-set-key (kbd "C-S-k") #'helm-find-files)


(global-set-key (kbd "C-.") #'helm-all-mark-rings)

(global-set-key (kbd "C-S-v") #'helm-show-kill-ring)
