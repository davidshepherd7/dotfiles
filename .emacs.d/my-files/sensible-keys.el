

;; Aim is to create a set of keybinds consistent with "normal"
;; applications.

;; Unfortunately this will be incompatible with standard emacs keybinds...


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
  ;; *next* block (if we were in whitespace between the two).
  (backward-paragraph)
  (forward-paragraph)
  ;; Now go past all the trailing whitespace too.
  (forward-line -1)
  (search-forward-regexp "\n\n+" nil repeat)
  (beginning-of-line))

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

(global-set-key (kbd "C-a") 'mark-whole-buffer)

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

;; Some standard things from e.g. chrome
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)

(global-set-key (kbd "C-w") 'kill-buffer)

(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "M-o") 'switch-to-buffer)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "M-f") 'isearch-forward-regexp)


;; Use vim/gmail/google-reader like up/down/left/right
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
(global-set-key (kbd "C-M-k") 'backward-sexp)


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
(global-set-key (kbd "M-\\") 'execute-extended-command) ;; also menu key
(global-set-key (kbd "M-'") 'capitalize-word) ;; no better ideas for this one...

(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "M-C-e") 'forward-paragraph)
(global-set-key (kbd "M-E") 'forward-sexp)

(global-set-key (kbd "C-b") 'smart-beginning-of-line)
(global-set-key (kbd "M-C-b") 'backward-paragraph)
(global-set-key (kbd "M-S-b") 'backward-sexp)

(global-set-key (kbd "C-n") 'newline-and-indent)
(global-set-key (kbd "M-n") 'newline-below-this-one)
(global-set-key (kbd "M-S-n") 'newline-above-this-one)

(defun newline-below-this-one ()
  "Open a newline underneath this line and move to it."
  (interactive) (end-of-line) (newline-and-indent))

(defun newline-above-this-one ()
  "Open a newline above this line and move to it."
  (interactive) (previous-line) (newline-below-this-one))

;; Block indent like python mode has for everywhere
(global-set-key (kbd "C-\\ >") 'py-shift-right)
(global-set-key (kbd "C-\\ <") 'py-shift-left)


;; New keymap for search
;; ============================================================

;; Changes for consistency:
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-'") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "C-j") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "C-k") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "C-y") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-q") 'isearch-quote-char)


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
(define-key isearch-mode-map (kbd "C-e") 'isearch-yank-line)

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
  (local-set-key (kbd "C-c") nil)
  (define-key flyspell-mode-map (kbd "C-c") nil)
  (local-set-key (kbd "C-x") nil)
  (local-set-key (kbd "C-j") nil)
  (local-set-key (kbd "C-y") nil)

  (local-set-key (kbd "<f5>") 'my-recompile)
  (local-set-key (kbd "<f6>") 'tex-view)

  ;; Blocks
  (local-set-key (kbd "M-]") 'latex-close-block)
  (local-set-key (kbd "M-[") 'latex-insert-block)
  ;; (local-set-key (kbd "") 'latex-split-block)

  ;; References
  (local-set-key (kbd "C-{") 'reftex-label)
  (local-set-key (kbd "C-}") 'reftex-reference)
  (local-set-key (kbd "C-]") 'reftex-citation)


  ;; Other stuff
  (local-set-key (kbd "M-RET") 'latex-insert-item)
  ;; (local-set-key (kbd "") 'tex-validate-buffer)
  (define-key tex-mode-map [remap forward-sexp] 'latex-forward-sexp)
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
;; ============================================================
(defun sensible-yas-mode-keys ()
  (interactive)

  ;; kill C-c
  (define-key yas-minor-mode-map (kbd "C-c") 'nil))

(when (boundp 'yas-minor-mode-hook)
  (add-hook 'yas-minor-mode-hook 'sensible-yas-mode-keys))


;; Haskell mode
;; ============================================================

(defun sensible-haskell-keys ()
  (interactive)

  (define-key haskell-mode-map (kbd "C-c") 'nil)
  (define-key haskell-mode-map (kbd "C-x") 'nil)

  (local-set-key (kbd "<f5>") 'my-recompile))

(add-hook 'haskell-mode-hook 'sensible-haskell-keys)


;; Note: we also have to mess around with some other mode's keybinds. In
;; particular:

;; Kill all keybinds in undo-tree (commented out the undo-tree-map).
;; Kill all keybinds in yas (not done yet).

;; Add command (use-local-map '()) to mode hooks (before your own keybinds)
;; remove all keybinds from a major mode.
