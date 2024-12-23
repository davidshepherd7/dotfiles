

;; Aim is to create a set of keybinds consistent with "normal"
;; applications.

;; Unfortunately this will be incompatible with standard emacs keybinds...

(setq colemak-mode 't)

;; unbind some standard keys
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-y"))
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-o"))

(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-M-o"))


;; kill emacs on a key is dangerous
(global-unset-key (kbd "C-x C-c"))

;; Don't have the suspend button somewhere that I can press it easily...
(global-set-key (kbd "C-x C-z") nil)

;; Unbind old case change keys
(global-unset-key (kbd "M-'"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-u"))



;; For operations on regions:
;; C-? : on region or to end of line
;; C-? : on region or to start of line
;; M-? : on region or all of line
;; M-S-? : on block (blankline seperated, incl. trailing newlines)
;; C-M-? : on function (or equvalent) -- disabled, use M-h to select block

(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun point-at-beginning-of-block (&optional repeat)
  (save-excursion (skip-to-next-blank-line)
                  (point)))

(defun point-at-end-of-block (&optional repeat)
  (save-excursion (skip-to-previous-blank-line)
                  (point)))

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


;; Cut
;; (global-set-key (kbd "C-x") (lambda () (interactive) (dwim-end-of-line 'kill-region)))
(global-set-key (kbd "M-x") (lambda () (interactive) (dwim-entire-line 'kill-region)))
(global-set-key (kbd "C-S-x") (lambda () (interactive) (dwim-start-of-line 'kill-region)))
;; (global-set-key (kbd "C-M-x") (lambda () (interactive) (function-on-block 'kill-region)))

;; Copy
;; (global-set-key (kbd "C-c") (lambda () (interactive) (dwim-end-of-line 'kill-ring-save)))
(global-set-key (kbd "M-c") (lambda () (interactive) (dwim-entire-line 'kill-ring-save)))
(global-set-key (kbd "C-S-c") (lambda () (interactive) (dwim-start-of-line 'kill-ring-save)))
;; (global-set-key (kbd "C-M-c") (lambda () (interactive) (function-on-block 'kill-ring-save)))

;; Paste (and cycle through pastes)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)

;; Undo (see also: undo-tree config)
(global-set-key (kbd "M-z") (lambda () (interactive)
                              (universal-argument)
                              (undo)
                              (setq deactivate-mark nil)))

;; Commenting
(global-set-key (kbd "C-;") (lambda () (interactive)
                              (dwim-end-of-line 'comment-or-uncomment-region)))
(global-set-key (kbd "M-;") (lambda () (interactive)
                              (dwim-entire-line 'comment-or-uncomment-region)))
;; Doesn't do anything useful...
;; (global-set-key (kbd "C-:") (lambda () (interactive)
;; (dwim-start-of-line 'comment-or-uncomment-region)))
;; (global-set-key (kbd "C-M-;") (lambda () (interactive)
                                ;; (function-on-block 'comment-or-uncomment-region)))



;; eval
(global-set-key (kbd "C-#") 'eval-region)

;; Some standard things from e.g. chrome
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)

(global-set-key (kbd "C-w") #'bury-buffer)
(global-set-key (kbd "M-w")
                (lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "C-S-w")
                (lambda () (interactive) (progn (kill-buffer (buffer-name))
                                                (delete-frame))))
;; Use (kill-buffer (buffer-name)) rather than (kill-this-buffer) because
;; something is stopping (kill-this-buffer) from working!

;; (global-set-key (kbd "C-o") 'find-file)
;; (global-set-key (kbd "M-o") 'switch-to-buffer)

(global-set-key (kbd "C-M-k") #'make-frame)


;; Use vim/gmail/google-reader like up/down/left/right
(if colemak-mode

    (progn
      (global-set-key (kbd "M-n") 'backward-char)
      (global-set-key (kbd "C-n") 'backward-word)
      (global-set-key (kbd "C-M-n") 'backward-sentence)

      (global-set-key (kbd "M-e") 'forward-char)
      (global-set-key (kbd "C-e") 'forward-word)
      (global-set-key (kbd "C-M-e") 'forward-sentence)

      (global-set-key (kbd "M-h") 'skip-to-next-blank-line)
      (global-set-key (kbd "C-h") 'next-line)
      (global-set-key (kbd "C-M-h") 'forward-sexp)

      (global-set-key (kbd "M-i") 'skip-to-previous-blank-line)
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

      (add-hook 'prog-mode-hook 'set-tab)
      (add-hook 'nxml-mode-hook 'set-tab)
      (add-hook 'org-mode-hook 'set-tab)
      (add-hook 'c++-mode-hook 'set-tab t)
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
    ;; I never use this, so taking it for something else:
    ;; (global-set-key (kbd "C-M-k") 'backward-sexp)

    ))


;; Disable insert key, because it sucks
(global-set-key (kbd "<insert>") nil)
(global-set-key (kbd "C-M-<insert>") #'overwrite-mode)


;; case changes
(global-set-key (kbd "C-/") 'capitalize-dwim)
(global-set-key (kbd "M-/") 'downcase-dwim)
(global-set-key (kbd "C-M-/") 'upcase-dwim)



;; Some deletion commands
(global-set-key (kbd "M-y") 'backward-delete-char)
(global-set-key (kbd "C-y") 'backward-kill-word)
(global-set-key (kbd "C-M-y") 'backward-kill-sexp)

(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "M-d") 'delete-char)
(global-set-key (kbd "C-M-d") 'kill-sexp)

;; Just because it's handy
(global-set-key (kbd "C-'") 'query-replace)
(global-set-key (kbd "M-'") 'query-replace-regexp)
(global-set-key (kbd "C-@") #'ds/query-replace-symbol-at-point)


;; Now assign new keys for the things we just wrote all over
(global-set-key (kbd "C-\\") ctl-x-map)
;; (global-set-key (kbd "C-\\ C-\\") 'exchange-point-and-mark)
;; (global-set-key (kbd "M-\\") 'execute-extended-command) ;; also menu key

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
(defun beginning-of-line-char (char)
  (save-excursion (goto-char char)
                  (point-at-bol)))
(defun indent-rigidly-dwim (distance)
  (if (region-active-p)
      (indent-rigidly (region-beginning) (region-end) distance)
    ;; else
    (indent-rigidly (point-at-bol) (point-at-eol) distance))
  (setq deactivate-mark nil))

(global-set-key (kbd "C-\\ C-.") (lambda () (interactive) (indent-rigidly-dwim 4)))
(global-set-key (kbd "C-\\ C-,") (lambda () (interactive) (indent-rigidly-dwim -4)))
(global-set-key (kbd "<C-S-iso-lefttab>") (lambda () (interactive) (indent-rigidly-dwim 4)))
(global-set-key (kbd "<backtab>") (lambda () (interactive) (indent-rigidly-dwim -4)))

(when ds/emacs-up-to-date?
  (global-set-key (kbd "C-<return>") #'rectangle-mark-mode)
  (global-set-key (kbd "M-SPC") #'cycle-spacing)
  (global-set-key (kbd "C-S-f") #'isearch-forward-symbol-at-point))


;; Keys so I don't need to let go of C during combos
(global-set-key (kbd "C-\\ C-#") 'server-edit)
(global-set-key (kbd "C-\\ C-h") 'mark-whole-buffer)


;; Utilities
(global-set-key (kbd "<f7>") 'projectile-vc)

;; paging
(defun window-half-height () (max 1 (/ (1- (window-height (selected-window))) 2)))
(defun scroll-down-half-screen ()
  (interactive)
  (scroll-down-command (window-half-height)))
(defun scroll-up-half-screen ()
  (interactive)
  (scroll-up-command (window-half-height)))

(global-set-key (kbd "C-<home>") #'recenter-top-bottom)
(global-set-key (kbd "C-<prior>") #'scroll-down-half-screen)
(global-set-key (kbd "C-<next>") #'scroll-up-half-screen)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;; Align
(define-key prog-mode-map (kbd "C-M-q") nil)
(global-set-key (kbd "C-M-q") #'align-current)


;; New keymap for search
;; ============================================================

(global-set-key (kbd "C-f") #'isearch-forward)
(global-set-key (kbd "M-f") (lambda () (interactive) (isearch-forward t)))
(global-set-key (kbd "C-r") #'isearch-backward)
(global-set-key (kbd "M-r") (lambda () (interactive) (isearch-backward t)))

;; Changes for consistency:
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-'") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "C-y") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-q") 'isearch-quote-char)

(if colemak-mode
    (progn
      (define-key isearch-mode-map (kbd "C-n") 'isearch-ring-advance)
      (define-key isearch-mode-map (kbd "C-e") 'isearch-ring-retreat)
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
(define-key isearch-mode-map (kbd "M-n") 'isearch-edit-string)

;; Grab text from the main buffer
(defun isearch-text-grab (movement-function)
  (isearch-yank-internal #'(lambda () (funcall movement-function) (point))))

(define-key isearch-mode-map (kbd "M-l")
            #'(lambda () (interactive) (isearch-text-grab 'forward-word)))
(define-key isearch-mode-map (kbd "C-l")
            #'(lambda () (interactive) (isearch-text-grab 'forward-char)))
(define-key isearch-mode-map (kbd "C-l") 'isearch-yank-line)

;; Remove keys that overlap main keymap in weird or useless ways
(define-key isearch-mode-map (kbd "C-h") nil)
(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-q") nil)
(define-key isearch-mode-map (kbd "C-l") nil)
;; (define-key isearch-mode-map (kbd "C-w") nil)
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
(setq gud-key-prefix "")


;; Org mode
;; ============================================================

(defun sensible-org-mode-keys ()
  (interactive)

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



;; emacs lisp mode
;; ============================================================

(defun sensible-emacs-lisp-keys ()
  (interactive)

  (local-unset-key (kbd "C-M-x")))
(add-hook 'emacs-lisp-mode-hook 'sensible-emacs-lisp-keys)



;; Note: we also have to mess around with some other mode's keybinds. In
;; particular:

;; Kill all keybinds in yas (not done yet).

;; Add command (use-local-map '()) to mode hooks (before your own keybinds)
;; remove all keybinds from a major mode.



;; Mark various regions
;; ============================================================

(global-set-key (kbd "C-o") 'mark-paragraph)

(global-set-key (kbd "C-a") 'er/expand-region)
(global-set-key (kbd "C-S-a") (lambda () (interactive) (er/expand-region -1)))



;; Help mode
;; ============================================================

(require 'help-mode)
(define-key help-mode-map (kbd "<M-left>") #'help-go-back)
(define-key help-mode-map (kbd "<M-right>") #'help-go-forward)


;; Minibuffer
;; ============================================================

(-each
    (list minibuffer-local-map minibuffer-local-completion-map minibuffer-local-ns-map )
  (lambda (map)
    (define-key map (kbd "M-n") 'nil)
    (define-key map (kbd "M-e") 'nil)))


;; Window management
;; ============================================================

(defun focus-active-minibuffer ()
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-\\ m") #'focus-active-minibuffer)


;; No more arrow keys!
;; ============================================================

(-each (list (kbd "<left>") (kbd "<down>") (kbd "<right>") (kbd "<up>")) #'global-unset-key)
