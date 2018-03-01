

(define-derived-mode emacs-anywhere-mode text-mode "Emacs Anywhere")
(add-to-list 'auto-mode-alist (cons "\\.emacs-anywhere-8c7ed5092fff$" #'emacs-anywhere-mode))

(with-eval-after-load 'evil-states
  (add-to-list 'evil-insert-state-modes 'emacs-anywhere-mode))
