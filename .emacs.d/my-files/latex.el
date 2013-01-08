;; Latex editing settings
;;================================================================

;; Compile to pdf by default
(setq TeX-PDF-mode t)

;; use evince for previewing
(setq TeX-view-program-selection '(((output-dvi style-pstricks) "Evince")
				   (output-dvi "Evince")
				   (output-pdf "Evince")
				   (output-html "xdg-open")))

;; Set up to sync positions between emacs and evince
;; (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;; (setq TeX-source-correlate-start-server t)

;; Tell auctex to look harder for the master document
(setq-default TeX-master nil)

;; Turn on reftex and integrate into auctex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; ;; Autocomplete maths in latex mode
;; this seems to cause issues - autocomplete adds in odd symbols...
;; (require 'ac-math)
;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
;; (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
;;   (setq ac-sources
;;      (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;;                ac-sources)))
;; (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
;; (ac-flyspell-workaround) ;; Make it work with flyspell active

;; Search and replace in all .tex files
(fset 'lit-review-get-files
      [?\M-x ?f ?i ?n ?d ?- ?n ?a ?m ?e ?- ?d ?i ?r ?e ?d return ?\C-a ?\C-k ?~ ?/ ?D ?r ?o ?p ?b ?o ?x ?/ ?p ?h ?d ?/ ?l ?i ?t ?e ?r ?a ?t ?u ?r ?e tab ?l ?a ?t ?e ?x tab return ?* ?. ?t ?e ?x return])


;; keyboard macros for save + run latex/bibtex
(fset 'my-bibtex
   [f11 ?\C-c ?\C-c ?B ?i ?b ?t ?e ?x return])

(fset 'latex-compile
   [?\C-u f11 ?\C-c ?\C-c ?b ?i ?b ?t ?e ?x return ?\C-c ?\C-c ?b ?i ?b ?t ?e ?x return ?\C-c ?\C-c ?l ?a ?t ?e ?x return ?\C-c ?\C-c ?l ?a ?t ?e ?x return])


;; Add latex specific keybinds
(defun my-builds-keybinds ()
  (interactive)
  (local-set-key (kbd "<f5>") 'latex-compile)
  (local-set-key (kbd "<f6>") 'my-bibtex))
(add-hook 'LaTeX-mode-hook 'my-builds-keybinds)

;; Flyspell mode on by default
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;; Let me write "'s
(add-hook 'Latex-mode-hook
	  '(local-set-key (kbd "C-?\"") (self-insert-command \")))