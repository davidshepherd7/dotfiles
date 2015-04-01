
(set 'helm-command-prefix-key "C-\\ c")

(use-package helm :ensure t :demand)
(use-package helm-projectile :ensure t)


(require 'helm)
(require 'helm-config)

(helm-mode 1)

(global-set-key (kbd "C-S-v") #'helm-show-kill-ring)

;; Use helm-buffers-list over helm-mini because with this setting it uses
;; "virtual buffers" (allows you to switch to buffers that you closed).
(setq ido-use-virtual-buffers t)

;; increase number of buffers to rememeber
(set 'recentf-max-saved-items 1000)

(global-set-key (kbd "M-k") #'helm-buffers-list)
(global-set-key (kbd "C-S-k") #'helm-find-files)

;; Use better colour for highlighting the current line
(set-face-attribute 'helm-selection nil :background "grey20")

(use-package helm-swoop
  :config (progn
            (global-set-key (kbd "M-F") #'helm-swoop)
            (define-key helm-swoop-edit-map (kbd "C-s") #'helm-swoop--edit-complete))
  :ensure t)


(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-\\") #'helm-M-x)


(global-set-key (kbd "C-.") #'helm-all-mark-rings)

;; Fuzzy matching settings
(setq helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
