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
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Add bibtex warnings to the compile mode highlighted errors
(when (boundp 'compilation-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(biber "^WARN \- .*$"))
  (add-to-list 'compilation-error-regexp-alist 'biber))


;; Flyspell mode on by default
(add-hook 'latex-mode-hook 'turn-on-flyspell)

;; Use isearch over whole document by default
(reftex-isearch-minor-mode t)

;; Use a more normal end of paragraph
(defun latex-normal-paragraph-starts ()
  (interactive)
  (setq paragraph-start "[      ]*$")
  (setq paragraph-separate paragraph-start)
)
(add-hook 'latex-mode-hook 'latex-normal-paragraph-starts)

;; use \eqref not (\ref) for equation references
(setq reftex-label-alist '(AMSTeX))

(set 'reftex-default-bibliography '("~/Documents/library.bib"))
