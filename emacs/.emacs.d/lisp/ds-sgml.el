
(require 'sgml-mode)

(use-package sgml-mode
  :config
  (add-hook 'sgml-mode-hook #'set-tab))

(defun insert-br-tag ()
  (interactive)
  (insert "<br>"))
(define-key html-mode-map (kbd "M-RET") #'insert-br-tag)

(require 'aggressive-indent)
(add-hook 'sgml-mode-hook #'aggressive-indent-mode)

(defun ds/switch-to-js ()
  (interactive)
  (--> '(".ts" ".js")
       (--map (concat (file-name-sans-extension (buffer-file-name)) it) it)
       (--first (f-exists? it) it)
       (find-file it)))
(define-key html-mode-map (kbd "C-\\ o") #'ds/switch-to-js)

(defun ds/html-switch-to-test ()
  (interactive)
  (--> '(".spec.ts" ".spec.js")
       (--map (concat (file-name-sans-extension (buffer-file-name)) it) it)
       (--first (f-exists? it) it)
       (find-file it)))
  (define-key html-mode-map (kbd "C-\\ n") #'ds/html-switch-to-test)


(define-key sgml-mode-map (kbd "C-M-n") #'sgml-skip-tag-forward)
(define-key sgml-mode-map (kbd "C-M-e") #'sgml-skip-tag-backward)

(define-key sgml-mode-map (kbd "<") nil)

(require 'flycheck)
(add-hook 'html-mode-hook #'flycheck-mode)
(add-hook 'css-mode-hook #'flycheck-mode)

(add-to-list 'auto-mode-alist (cons "\\.wxs" #'xml-mode))
(add-to-list 'auto-mode-alist (cons "\\.mustache$" #'html-mode))

(defun ds/sgml-pretty-print (beg end)
  "Simple-minded pretty printer for SGML.
Re-indents the code and inserts newlines between BEG and END.
You might want to turn on `auto-fill-mode' to get better results.

Taken from sgml-pretty-print, hacked to not add newlines before end tags."
  (interactive "r")
  (save-excursion
    (if (< beg end)
        (goto-char beg)
      (goto-char end)
      (setq end beg)
      (setq beg (point)))
    ;; Don't use narrowing because it screws up auto-indent.
    (setq end (copy-marker end t))
    (with-syntax-table sgml-tag-syntax-table
      (while (re-search-forward "<" end t)
        (goto-char (match-beginning 0))
        (unless (or (and (looking-at "</") (looking-back "<[^/>]*>[^<]*"))
                    (progn (skip-chars-backward " \t") (bolp)))
          (reindent-then-newline-and-indent))
        (sgml-forward-sexp 1)))
    ))


(defun ds/sgml-pretty-print-buffer ()
  (interactive)
  (ds/sgml-pretty-print (point-min) (point-max)))
