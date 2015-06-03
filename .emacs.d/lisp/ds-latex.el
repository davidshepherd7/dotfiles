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

;; Use isearch over whole document by default
(reftex-isearch-minor-mode t)

;; Keep whitespace clean
(defun remap-save-clean-whitespace-latex ()
  (interactive)
  (local-set-key [remap save-buffer] 'clean-whitespace-and-save))
(add-hook 'LaTeX-mode-hook 'remap-save-clean-whitespace-latex)

;; ;; Use a more normal end of paragraph
;; (defun latex-normal-paragraph-starts ()
;;   (interactive)
;;   (setq paragraph-start "[      ]*$")
;;   (setq paragraph-separate paragraph-start)
;; )
;; (add-hook 'latex-mode-hook 'latex-normal-paragraph-starts)

;; use \eqref not (\ref) for equation references
(setq reftex-label-alist '(AMSTeX))

(set 'reftex-default-bibliography '("~/Documents/library.bib"))

;; highlight new macros '\\\\' is double escaped \: escaped once for regex,
;; once for emacs.
(font-lock-add-keywords 'latex-mode
                        '(
                          ;; ("\\\\\[cC]ref" . 'font-lock-keyword-face)
                          ;; ("\\\\[tT]hisref" . 'font-lock-keyword-face)
                          ("\\\\includegraphics" . 'font-lock-keyword-face)
                          ("\\\\newsubcommand" . 'font-lock-keyword-face)
                          )
                        )

;; For some reason lots of things aren't bound at startup, so put them in
;; this thingy.
(eval-after-load
    'tex
  '(progn (set 'font-latex-match-reference-keywords
               '(("cref" "[{")
                 ("Cref" "[{")
                 ("thisref" "[{")
                 ("Thisref" "[{")
                 ))

          (define-key TeX-mode-map [f6] #'evince-tex-main)
          (define-key LaTeX-mode-map [f6] #'evince-tex-main)))

(defun latex-insert-last-label (nprev)
  "Insert clever reference to most recent (by position in buffer) label. If prefix
arg is set go that many labels backward, if negative then go
forward instead."
  (interactive "p") ;; take prefix arg as input to function

  ;; search for last use of \label
  (save-excursion (re-search-backward "\\\\label\\({[^}]*}\\)" nil nil nprev))

  ;; insert the match
  (insert (match-string 1)))


(defun ds/evince-tex-main ()
  "View current tex-main's pdf in evince"
  (interactive)
  (start-process "view-latex-pdf" nil "evince"
                 (s-concat (file-name-sans-extension (tex-main-file)) ".pdf")))

;; keybinds
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "C-.") 'latex-insert-last-label)
            (local-set-key [f6] #'ds/evince-tex-main)
            (set-tab)) t)

;; disable aggressive indent mode
(when (boundp 'aggressive-indent-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (aggressive-indent-mode 0))))



;; flyspell
;; ============================================================

;; always use flyspell in latex
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; tell flyspell what to ignore
(set 'flyspell-tex-command-regexp "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|label\\|ref\\|cref\\|Cref\\|[tT]hisref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")

;; clear flyspell bindings
(set 'flyspell-mode-map (make-sparse-keymap))
