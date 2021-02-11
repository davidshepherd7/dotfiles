
;; Still need this for a few things
(use-package helm)

(use-package ivy
  :diminish "ivy"
  :config

  (require 'ivy)

  (ivy-mode)
  (global-set-key (kbd "C-|") #'ivy-resume)

  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-m") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-immediate-done)

  (define-key ivy-minibuffer-map (kbd "C-p") #'ivy-avy)

  (define-key ivy-minibuffer-map (kbd "C-i") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-M-i") #'ivy-previous-line-and-call)
  (define-key ivy-minibuffer-map (kbd "C-h") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-M-h") #'ivy-next-line-and-call)

  (define-key ivy-minibuffer-map (kbd "<C-tab>") #'ivy-insert-current)

  (define-key ivy-minibuffer-map (kbd "M-n") nil)


  (defun ivy-partial-or-next-line ()
    "Complete the minibuffer text as much as possible.
  If the text hasn't changed as a result, forward to `ivy-next-line'."
    (interactive)
    (or (ivy-partial) (ivy-next-line)))
  (define-key ivy-minibuffer-map (kbd "<backtab>") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-partial-or-next-line)

  (validate-setq ivy-use-virtual-buffers t)
  (validate-setq ivy-height 30)
  (validate-setq ivy-wrap t)
  (validate-setq ivy-on-del-error-function #'ignore)

  (validate-setq ivy-extra-directories nil)

  (validate-setq ivy-re-builders-alist
                 '((t . ivy--regex-ignore-order)))

  (validate-setq ivy-initial-inputs-alist '())


  ;; Sort matches by longest match first
  (defun ds/ivy-sort-by-length (_name candidates)
    (if (> (length candidates) 200)
        candidates
      (cl-sort (copy-sequence candidates)
               (lambda (f1 f2)
                 (< (length f1) (length f2))))))

  (setf (alist-get t ivy-sort-matches-functions-alist) #'ds/ivy-sort-by-length)
  )

(use-package ivy-hydra)

(use-package counsel
  :config

  (global-set-key (kbd "M-\\") #'counsel-M-x)
  (global-set-key (kbd "<menu>") #'counsel-M-x)
  (global-set-key (kbd "<apps>") #'counsel-M-x)
  (global-set-key (kbd "M-k") #'switch-to-buffer)
  (global-set-key (kbd "C-S-k") #'counsel-find-file)
  (global-set-key (kbd "C-.") #'counsel-mark-ring)
  (global-set-key (kbd "C-S-v") #'counsel-yank-pop)
  (global-set-key (kbd "C-=") #'counsel-bookmark)
  )

(use-package counsel-projectile
  :config

  (with-eval-after-load 'projectile
    (validate-setq projectile-completion-system 'ivy))

  (global-set-key (kbd "C-k") #'counsel-projectile-find-file)
  )

;; I think this was consuming so much RAM that it forced constant garbage
;; collection!

;; (use-package ivy-prescient
;;   :config
;;   (ivy-prescient-mode 0)
;;   )
