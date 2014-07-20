

;; Aim is to create a set of keybinds consistent with "normal"
;; applications.

;; Unfortunately this will be incompatible with standard emacs keybinds...

(setq colemak-mode 't)

;; unbind some standard keys
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-y"))

(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-unset-key (kbd "C-M-l"))

;; For operations on regions:
;; C-? : on region or to end of line
;; C-? : on region or to start of line
;; M-? : on region or all of line
;; M-S-? : on block (blankline seperated, incl. trailing newlines)
;; C-M-? : on function (or equvalent)


(defun go-to-beginning-of-block (&optional repeat)
  (interactive)
  (backward-paragraph)
  (next-line 1)
  (beginning-of-line))

(defun point-at-beginning-of-block (&optional repeat)
  (save-excursion (go-to-beginning-of-block) (point)))

(defun go-to-end-of-block (&optional repeat)
  (interactive)
  ;; Go to start first to make sure we don't end up at the end of the
  ;; *next* block (if we were in whitespace between the two). Then go to
  ;; the end of the text in this paragraph (usually whitespace delimited).
  (backward-paragraph)
  (forward-paragraph)

  ;; Now go past all the trailing whitespace too. If we hit the end of the
  ;; buffer during this search then the end of the buffer is the end of the
  ;; block.
  (forward-line -1)
  (unless (ignore-errors (search-forward-regexp "\n\n+" nil repeat))
    (goto-char (point-max))))

(defun point-at-end-of-block (&optional repeat)
  (save-excursion (go-to-end-of-block) (point)))

(defun point-at-beginning-of-next-line ()
  (save-excursion (end-of-line) (forward-line)
                  (point)))


(defun dwim-end-of-line (function)
  "If a region is selected operate on it. Otherwise operate on
point to end of line. If point is at end of line then operate on
the line break."
  (if (region-active-p)
      (funcall function (region-beginning) (region-end))
    ;; else
    (if (eq (point) (point-at-eol))
        (funcall function (point) (point-at-beginning-of-next-line))
      ;; else
      (funcall function (point) (point-at-eol)))))

(defun dwim-start-of-line (function)
  (if (region-active-p)
      (funcall function (region-beginning) (region-end))
    (funcall function (point) (point-at-bol))))

(defun dwim-entire-line (function)
  (if (region-active-p)
      (funcall function (region-beginning) (region-end))
    ;; else
    (funcall function (point-at-bol) (point-at-beginning-of-next-line))))

(defun function-on-block (function)
  (funcall function (point-at-beginning-of-block)
           (point-at-end-of-block))
  ;; ;; then go to the next block
  ;; (forward-paragraph)
  ;; (forward-line 1)
  )


;; CUA keybinds (undo/cut/copy/paste)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; Cut
(global-set-key (kbd "C-x") (lambda () (interactive) (dwim-end-of-line 'kill-region)))
(global-set-key (kbd "M-x") (lambda () (interactive) (dwim-entire-line 'kill-region)))
(global-set-key (kbd "C-S-x") (lambda () (interactive) (dwim-start-of-line 'kill-region)))
(global-set-key (kbd "C-M-x") (lambda () (interactive) (function-on-block 'kill-region)))

;; Copy
(global-set-key (kbd "C-c") (lambda () (interactive) (dwim-end-of-line 'kill-ring-save)))
(global-set-key (kbd "M-c") (lambda () (interactive) (dwim-entire-line 'kill-ring-save)))
(global-set-key (kbd "C-S-c") (lambda () (interactive) (dwim-start-of-line 'kill-ring-save)))
(global-set-key (kbd "C-M-c") (lambda () (interactive) (function-on-block 'kill-ring-save)))

;; Paste (and cycle through pastes)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)

;; Commenting
(global-set-key (kbd "C-;") (lambda () (interactive)
                              (dwim-end-of-line 'comment-or-uncomment-region)))
(global-set-key (kbd "M-;") (lambda () (interactive)
                              (dwim-entire-line 'comment-or-uncomment-region)))
;; Doesn't do anything useful...
;; (global-set-key (kbd "C-:") (lambda () (interactive)
;; (dwim-start-of-line 'comment-or-uncomment-region)))
(global-set-key (kbd "C-M-;") (lambda () (interactive)
                                (function-on-block 'comment-or-uncomment-region)))


;; eval
(global-set-key (kbd "C-#") (lambda () (interactive) (dwim-end-of-line 'eval-region)))
(global-set-key (kbd "M-#") (lambda () (interactive) (dwim-entire-line 'eval-region)))
(global-set-key (kbd "C-S-#") (lambda () (interactive) (dwim-start-of-line 'eval-region)))
(global-set-key (kbd "C-M-#") (lambda () (interactive) (function-on-block 'eval-region)))


;; Some standard things from e.g. chrome
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)

(global-set-key (kbd "C-w") 
                (lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "C-S-w") 
                (lambda () (interactive) (progn (kill-buffer (buffer-name))
                                                (delete-frame))))
;; Use (kill-buffer (buffer-name)) rather than (kill-this-buffer) because
;; something is stopping (kill-this-buffer) from working!

;; (global-set-key (kbd "C-o") 'find-file)
;; (global-set-key (kbd "M-o") 'switch-to-buffer)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "M-f") 'isearch-forward-regexp)


;; Use vim/gmail/google-reader like up/down/left/right
(if colemak-mode

    (progn
      (global-set-key (kbd "M-n") 'backward-char)
      (global-set-key (kbd "C-n") 'backward-word)
      (global-set-key (kbd "C-M-n") 'backward-sentence)

      (global-set-key (kbd "M-o") 'forward-char)
      (global-set-key (kbd "C-o") 'forward-word)
      (global-set-key (kbd "C-M-o") 'forward-sentence)

      (global-set-key (kbd "M-e") 'forward-paragraph)
      (global-set-key (kbd "C-e") 'next-line)
      (global-set-key (kbd "C-M-e") 'forward-sexp)

      (global-set-key (kbd "M-i") 'backward-paragraph)
      (global-set-key (kbd "C-i") 'previous-line)
      (global-set-key (kbd "C-M-i") 'backward-sexp)

      ;; We just stomped tab via C-i so rebind it, have to use [tab] not
      ;; (kbd "TAB") so that we only bind tab itself and not C-i as
      ;; well. But only in certain modes otherwise we stomp the
      ;; autocomplete in minibuffer (there should be an easier way to do
      ;; this..).
      (defun set-tab () 
        (interactive)
        (local-set-key [tab] 'indent-for-tab-command))
      (add-hook 'c-mode-common-hook 'set-tab)
      (add-hook 'python-mode-hook 'set-tab)
      (add-hook 'LaTeX-mode-hook 'set-tab)
      (add-hook 'latex-mode-hook 'set-tab)
      (add-hook 'lisp-mode-hook 'set-tab)
      (add-hook 'emacs-lisp-mode-hook 'set-tab)
      (add-hook 'shell-lisp-mode-hook 'set-tab)
      (add-hook 'haskell-mode-hook 'set-tab)

      (global-set-key (kbd "C-k") 'find-file)
      (global-set-key (kbd "M-k") 'switch-to-buffer)
      )


  ;; else
  (progn
    (global-set-key (kbd "M-h") 'backward-char)
    (global-set-key (kbd "C-h") 'backward-word)
    (global-set-key (kbd "C-M-h") 'backward-sentence)

    (global-set-key (kbd "M-l") 'forward-char)
    (global-set-key (kbd "C-l") 'forward-word)
    (global-set-key (kbd "C-M-l") 'forward-sentence)

    ;; Even though it's a bit inconsistent use C-j/k for single lines because
    ;; it's easier!
    (global-set-key (kbd "M-j") 'forward-paragraph)
    (global-set-key (kbd "C-j") 'next-line)
    (global-set-key (kbd "C-M-j") 'forward-sexp)

    (global-set-key (kbd "M-k") 'backward-paragraph)
    (global-set-key (kbd "C-k") 'previous-line)
    (global-set-key (kbd "C-M-k") 'backward-sexp)))

;; case changes
(global-set-key (kbd "C-/") 'capitalize-word)
(global-set-key (kbd "M-/") 'downcase-word)
(global-set-key (kbd "C-M-/") 'upcase-word)



;; Some deletion commands
(global-set-key (kbd "M-y") 'backward-delete-char)
(global-set-key (kbd "C-y") 'backward-kill-word)
(global-set-key (kbd "C-M-y") 'backward-kill-sexp)

(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "M-d") 'delete-char)
(global-set-key (kbd "C-M-d") 'kill-sexp)

;; Just because it's handy
(global-set-key (kbd "C-'") 'query-replace)


;; Now assign new keys for the things we just wrote all over
(global-set-key (kbd "C-\\") ctl-x-map)
(global-set-key (kbd "C-\\ C-\\") 'exchange-point-and-mark)
(global-set-key (kbd "M-\\") 'execute-extended-command) ;; also menu key
(global-set-key (kbd "M-'") 'capitalize-word) ;; no better ideas for this one...

;; Don't have the suspend button somewhere that I can press it easily...
(global-set-key (kbd "C-\\ C-z") nil)

(global-set-key (kbd "C-l") 'end-of-line)
(global-set-key (kbd "M-C-l") 'forward-paragraph)
(global-set-key (kbd "M-L") 'forward-sexp)

(global-set-key (kbd "C-b") 'smart-beginning-of-line)
(global-set-key (kbd "M-C-b") 'backward-paragraph)
(global-set-key (kbd "M-S-b") 'backward-sexp)

(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-j") 'newline-below-this-one)
(global-set-key (kbd "M-S-j") 'newline-above-this-one)

(defun newline-below-this-one ()
  "Open a newline underneath this line and move to it."
  (interactive) (end-of-line) (newline-and-indent))

(defun newline-above-this-one ()
  "Open a newline above this line and move to it."
  (interactive) (previous-line) (newline-below-this-one))

;; Block indent like python mode has for everywhere
(global-set-key (kbd "C-\\ C-.") 'py-shift-right)
(global-set-key (kbd "C-\\ C-,") 'py-shift-left)


;; Keys so I don't need to let go of C during combos
;; ============================================================
(global-set-key (kbd "C-\\ C-#") 'server-edit)
(global-set-key (kbd "C-\\ C-h") 'mark-whole-buffer)


;; New keymap for search
;; ============================================================

;; Changes for consistency:
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-'") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "C-y") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-q") 'isearch-quote-char)

(if colemak-mode
    (progn
      (define-key isearch-mode-map (kbd "C-e") 'isearch-ring-advance)
      (define-key isearch-mode-map (kbd "C-i") 'isearch-ring-retreat)
      )
  (progn 
    (define-key isearch-mode-map (kbd "C-j") 'isearch-ring-advance)
    (define-key isearch-mode-map (kbd "C-k") 'isearch-ring-retreat)))

;; (define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (kbd "RET") 'isearch-exit)
;; (define-key isearch-mode-map (kbd "C-g") 'isearch-abort)

;; Make deleting and paste work more like expected
(define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-DEL") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-pop)

;; Get to edit the search string properly
(define-key isearch-mode-map (kbd "M-e") 'isearch-edit-string)

;; Grab text from the main buffer
(defun isearch-text-grab (movement-function)
  (isearch-yank-internal '(lambda () (funcall movement-function) (point))))

(define-key isearch-mode-map (kbd "M-l")
  '(lambda () (interactive) (isearch-text-grab 'forward-word)))
(define-key isearch-mode-map (kbd "C-l")
  '(lambda () (interactive) (isearch-text-grab 'forward-char)))
(define-key isearch-mode-map (kbd "C-l") 'isearch-yank-line)

;; Remove keys that overlap main keymap in weird or useless ways
(define-key isearch-mode-map (kbd "C-h") nil)
(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-q") nil)
(define-key isearch-mode-map (kbd "C-l") nil)
;; (define-key isearch-mode-map (kbd "C-w") nil)
(define-key isearch-mode-map (kbd "C-x") nil)
(define-key isearch-mode-map (kbd "C-y") nil)
(define-key isearch-mode-map (kbd "C-z") nil)
(define-key isearch-mode-map (kbd "M-y") nil)
(define-key isearch-mode-map (kbd "ESC") nil)
(define-key isearch-mode-map (kbd "<escape>") nil)
(define-key isearch-mode-map (kbd "<mouse-2>") nil)
(define-key isearch-mode-map (kbd "C-^") nil)
(define-key isearch-mode-map (kbd "C-_") nil)
(define-key isearch-mode-map (kbd "C-?") nil)
(define-key isearch-mode-map (kbd "S-SPC") nil)
(define-key isearch-mode-map (kbd "C-\\") nil)
(define-key isearch-mode-map (kbd "C-]") nil)


;; New keys for gdb/gud mode
;; ============================================================

;; Unbind ctrl-p so that we can use it for the gud prefix command
(global-unset-key (kbd "C-p"))
(setq gud-key-prefix "")


;; Auctex and reftex
;; ============================================================
(defun sensible-latex-keys ()
  (interactive)

  ;; Unbind keys we use elsewhere
  (define-key LaTeX-mode-map (kbd "C-c") nil)
  (define-key LaTeX-mode-map (kbd "C-x") nil)

  ;; (define-key flyspell-mode-map (kbd "C-c") nil)
  ;; (define-key flyspell-mode-map (kbd "C-;") nil)

  (local-set-key (kbd "C-j") nil)
  (local-set-key (kbd "C-y") nil)

  (local-set-key (kbd "<f6>") 'tex-view)

  ;; Blocks
  (local-set-key (kbd "M-]") 'LaTeX-close-environment)
  (local-set-key (kbd "M-[") 'LaTeX-environment)

  ;; References
  (local-set-key (kbd "C-{") 'reftex-label)
  (local-set-key (kbd "C-}") 'reftex-reference)
  (local-set-key (kbd "C-]") 'reftex-citation)


  ;; Other stuff
  (local-set-key (kbd "M-RET") 'LaTeX-insert-item)
  ;; (local-set-key (kbd "") 'tex-validate-buffer)
  (define-key LaTeX-mode-map [remap forward-sexp] 'latex-forward-sexp)
  ;; (local-set-key (kbd "") 'reftex-create-tags-file)
  (local-set-key (kbd "C-#") 'reftex-toc))

;; Only add the keybind if the mode exists
(when (boundp 'latex-mode-hook)
  (add-hook 'latex-mode-hook 'sensible-latex-keys 't))

(when (boundp 'LaTeX-mode-hook)
  (add-hook 'LaTeX-mode-hook 'sensible-latex-keys 't))


;; If reftex exists then also null the C-c keybinds there
(when (boundp 'reftex-mode-hook)
  (add-hook 'reftex-mode-hook
	    (lambda () (interactive)
	      (define-key reftex-mode-map (kbd "C-c") nil))))



;; Org mode
;; ============================================================

(defun sensible-org-mode-keys ()
  (interactive)

  ;; kill C-c
  (define-key org-mode-map (kbd "C-c") 'nil)

  ;; kill some other over writing commands
  (define-key org-mode-map (kbd "C-k") 'nil)
  (define-key org-mode-map (kbd "C-j") 'nil)


  ;; (define-key org-mode-map (kbd "") )

  ;; Org mode's own versions of commands
  (define-key org-mode-map [remap yank] 'org-yank)
  (define-key org-mode-map [remap newline-and-indent] 'org-return-indent)
  (define-key org-mode-map [remap beginning-of-line] 'org-beginning-of-line)
  (define-key org-mode-map [remap end-of-line] 'org-end-of-line)


  ;; (define-key org-mode-map ( 'org-end-of-line))

  )

(when (boundp 'org-mode-hook)
  (add-hook 'org-mode-hook 'sensible-org-mode-keys))


;; yas mode

;; (defun sensible-yas-mode-keys ()
;;   (interactive)

;;   ;; kill C-c
;;   (define-key yas-minor-mode-map (kbd "C-c") 'nil))

;; (when (boundp 'yas-minor-mode-hook)
;;   (add-hook 'yas-minor-mode-hook 'sensible-yas-mode-keys))


;; Haskell mode
;; ============================================================

(defun sensible-haskell-keys ()
  (interactive)

  (define-key haskell-mode-map (kbd "C-c") 'nil)
  (define-key haskell-mode-map (kbd "C-x") 'nil)

  (local-set-key (kbd "<f5>") 'my-recompile))

(add-hook 'haskell-mode-hook 'sensible-haskell-keys)


;; emacs lisp mode
;; ============================================================

(defun sensible-emacs-lisp-keys ()
  (interactive)

  (local-unset-key (kbd "C-M-x")))
(add-hook 'emacs-lisp-mode-hook 'sensible-emacs-lisp-keys)



;; Note: we also have to mess around with some other mode's keybinds. In
;; particular:

;; Kill all keybinds in undo-tree (commented out the undo-tree-map).
;; Kill all keybinds in yas (not done yet).

;; Add command (use-local-map '()) to mode hooks (before your own keybinds)
;; remove all keybinds from a major mode.


;; Bash
;; ============================================================


(defun sensible-bash-keys ()
  (interactive)

  (define-key sh-mode-map (kbd "C-c") 'nil)
  (define-key sh-mode-map (kbd "C-x") 'nil)

  (local-set-key (kbd "<f5>") 'my-recompile))

(add-hook 'sh-mode-hook 'sensible-bash-keys)


;; Expand region
;; ============================================================

(global-set-key (kbd "C-a") 'er/expand-region)
(global-set-key (kbd "C-S-a") (lambda () (interactive) (er/expand-region -1)))



;; File open keybinds
;; ===============================================================
(global-set-key (kbd "C-<f12>")
                '(lambda () (interactive) (find-file "~/.zshrc")))
(global-set-key (kbd "C-<f11>")
                '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-<f10>")
                '(lambda () (interactive) (find-file "~/.emacs.d/skeletons.el")))
(global-set-key (kbd "C-<f9>")
                '(lambda () (interactive) (find-file "~/.emacs.d/abbrev_defs")))
(global-set-key (kbd "C-<f8>")
                '(lambda () (interactive) (find-file "~/.xmonad/xmonad.hs")))
(global-set-key (kbd "C-<f6>") 'ibuffer)
