

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
  (search-forward-regexp "\n\n+" nil repeat)
  (beginning-of-line))

(defun point-at-end-of-block (&optional repeat)
  (save-excursion (go-to-end-of-block) (point)))


(defun dwim-end-of-line (function)
  "If a region is selected operate on it. Otherwise operate on
point to end of line. If point is at end of line then operate on
the line break."
  (if (region-active-p)
      (funcall function (region-beginning) (region-end))
    ;; else
    (if (eq (point) (point-at-eol))
        (funcall function (point) (save-excursion (forward-line) (beginning-of-line) (point)))
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
    (if (eq (point-at-bol) (point-at-eol))
        (funcall function (point) (save-excursion (forward-line) (beginning-of-line) (point)))
      (funcall function (point-at-bol) (point-at-eol)))))

(defun function-on-block (function)
  (funcall function (point-at-beginning-of-block)
           (point-at-end-of-block)))


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

(global-set-key (kbd "C-S-o") 'find-file)
(global-set-key (kbd "C-o") 'switch-to-buffer)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "M-f") 'isearch-forward-regexp)


;; Use vim/gmail/google-reader like up/down/left/right
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "C-M-h") 'backward-sentence)

(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "C-M-l") 'forward-sentence)

(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "M-j") 'forward-paragraph)
(global-set-key (kbd "C-M-j") 'forward-sexp)

(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "M-k") 'backward-paragraph)
(global-set-key (kbd "C-M-k") 'backward-sexp)

;; Just because they're useful:
(global-set-key (kbd "C-y") 'backward-kill-word)
(global-set-key (kbd "C-'") 'query-replace)


;; Now assign new keys for the things we just wrote all over
(global-set-key (kbd "C-\\") ctl-x-map)
(global-set-key (kbd "M-\\") 'execute-extended-command)
(global-set-key (kbd "M-'") 'capitalize-word) ;; no better ideas for this one...

(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "M-C-e") 'forward-paragraph)
(global-set-key (kbd "M-E") 'forward-sexp)

(global-set-key (kbd "C-b") 'smart-beginning-of-line)
(global-set-key (kbd "M-C-b") 'backward-paragraph)
(global-set-key (kbd "M-B") 'backward-sexp)


(global-set-key (kbd "C-n") 'newline-and-indent)


;; New keymap for search
;; ============================================================

;; Changes for consistency:
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-'") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "C-j") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "C-k") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "C-y") 'isearch-del-char)

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
(define-key isearch-mode-map (kbd "M-l") 'isearch-yank-word)
(define-key isearch-mode-map (kbd "C-e") 'isearch-yank-line)

;; Remove keys that overlap main keymap in weird or useless ways
(define-key isearch-mode-map (kbd "C-h") nil)
(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-q") nil)
(define-key isearch-mode-map (kbd "C-l") nil)
(define-key isearch-mode-map (kbd "C-w") nil)
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



;; Note: we also have to mess around with some other mode's keybinds. In
;; particular:

;; Kill all keybinds in undo-tree (commented out the undo-tree-map).
;; Kill all keybinds in yas (not done yet).

;; Add command (use-local-map '()) to mode hooks (before your own keybinds)
;; remove all keybinds from a major mode.
