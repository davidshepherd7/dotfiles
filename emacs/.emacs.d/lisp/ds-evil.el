(use-package evil
  :ensure t
  :config

  ;; TODO

  ;; Case change

  ;; Auto indent insertion

  ;; Insert after character

  ;; Insert newlines before/after line

  ;; Replace in region

  ;; Insert new comment

  ;; Goto line number?

  ;; Vimify delete/backspace?

  ;; use vim sneak instead of search? Or vimify isearch somehow...


  ;; Maybe use:

  ;; 							*f*
  ;; f{char}			To [count]'th occurrence of {char} to the right.  The
  ;; 			cursor is placed on {char} |inclusive|.
  ;; 			{char} can be entered as a digraph |digraph-arg|.
  ;; 			When 'encoding' is set to Unicode, composing
  ;; 			characters may be used, see |utf-8-char-arg|.
  ;; 			|:lmap| mappings apply to {char}.  The CTRL-^ command
  ;; 			in Insert mode can be used to switch this on/off
  ;; 			|i_CTRL-^|.


  ;; 							*F*
  ;; F{char}			To the [count]'th occurrence of {char} to the left.
  ;; 			The cursor is placed on {char} |exclusive|.
  ;; 			{char} can be entered like with the |f| command.


  ;; 							*t*
  ;; t{char}			Till before [count]'th occurrence of {char} to the
  ;; 			right.  The cursor is placed on the character left of
  ;; 			{char} |inclusive|.
  ;; 			{char} can be entered like with the |f| command.


  ;; 							*T*
  ;; T{char}			Till after [count]'th occurrence of {char} to the
  ;; 			left.  The cursor is placed on the character right of
  ;; 			{char} |exclusive|.
  ;; 			{char} can be entered like with the |f| command.

  ;; Some bindings for these here
  (require 'projectile)
  (require 'hydra)

  ;; Globally enable evil mode
  (evil-mode 1)

  (setq evil-default-state 'normal)

  ;; remove all keybindings from most state keymaps
  (setcdr evil-insert-state-map nil)
  (setcdr evil-normal-state-map nil)
  (setcdr evil-motion-state-map nil)

  ;; ways to get back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "<f35>") 'evil-normal-state)

  ;; Coloured modeline when in insert mode
  ;; ============================================================
  (defvar ds/mode-line-default-background (face-attribute 'mode-line :background))
  (set 'ds/mode-line-evil-insert-background "#002900")
  (defun ds/set-mode-line-background (colour)
    (set-face-attribute 'mode-line nil :background colour))

  (add-hook 'evil-insert-state-entry-hook
            (lambda () (interactive)
              (ds/set-mode-line-background ds/mode-line-evil-insert-background)))

  (add-hook 'evil-insert-state-exit-hook
            (lambda () (interactive)
              (ds/set-mode-line-background ds/mode-line-default-background)))


  ;; Movement
  ;; ============================================================

  ;;; up/down/left/right
  (define-key evil-motion-state-map (kbd "i") #'evil-previous-line)
  (define-key evil-motion-state-map (kbd "h") #'evil-next-line)
  (define-key evil-motion-state-map (kbd "n") #'evil-backward-word-begin)
  (define-key evil-motion-state-map (kbd "e") #'evil-forward-word-begin)
  (define-key evil-motion-state-map (kbd "N") #'evil-backward-WORD-begin)
  (define-key evil-motion-state-map (kbd "E") #'evil-forward-WORD-begin)
  (define-key evil-motion-state-map (kbd "C-n") #'evil-backward-char)
  (define-key evil-motion-state-map (kbd "C-e") #'evil-forward-char)

  (define-key evil-motion-state-map (kbd "l") #'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "b") #'evil-beginning-of-line)

  (define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
  (define-key evil-motion-state-map (kbd "P") #'avy-goto-line)

  (define-key evil-motion-state-map "1" 'digit-argument)
  (define-key evil-motion-state-map "2" 'digit-argument)
  (define-key evil-motion-state-map "3" 'digit-argument)
  (define-key evil-motion-state-map "4" 'digit-argument)
  (define-key evil-motion-state-map "5" 'digit-argument)
  (define-key evil-motion-state-map "6" 'digit-argument)
  (define-key evil-motion-state-map "7" 'digit-argument)
  (define-key evil-motion-state-map "8" 'digit-argument)
  (define-key evil-motion-state-map "9" 'digit-argument)

  (define-key evil-motion-state-map (kbd "u") #'evil-forward-paragraph)
  (define-key evil-motion-state-map (kbd "y") #'evil-backward-paragraph)

  (evil-define-motion evil-end-of-buffer (count) (end-of-buffer))
  (define-key evil-motion-state-map (kbd "M->") #'evil-end-of-buffer)
  (evil-define-motion evil-beginning-of-buffer (count) (beginning-of-buffer))
  (define-key evil-motion-state-map (kbd "M-<") #'evil-beginning-of-buffer)

  (define-key evil-motion-state-map (kbd "(") #'evil-previous-open-paren)
  (define-key evil-motion-state-map (kbd ")") #'evil-next-close-paren)

  (define-key evil-motion-state-map (kbd "{") #'evil-previous-open-brace)
  (define-key evil-motion-state-map (kbd "}") #'evil-next-close-brace)


  ;; Actions
  ;; ============================================================

  (define-key evil-normal-state-map (kbd "c") #'evil-yank)

  (define-key evil-normal-state-map (kbd "d") #'evil-delete)

  (define-key evil-normal-state-map (kbd "s") #'evil-change)

  (define-key evil-normal-state-map (kbd "<tab>") #'evil-indent)

  (define-key evil-normal-state-map (kbd "<") #'evil-shift-left)
  (define-key evil-normal-state-map (kbd ">") #'evil-shift-right)

  (evil-define-operator evil-comment (beg end)
    :move-point nil
    (comment-or-uncomment-region beg end))
  (define-key evil-normal-state-map (kbd ";") #'evil-comment)

  (evil-define-operator evil-eval (beg end)
    :move-point nil
    ;; t: print to stdout
    (eval-region beg end t))
  (define-key evil-normal-state-map (kbd "#") #'evil-eval)

  ;; (evil-define-operator evil-query-replace (beg end from-string to-string)
  ;;   (call-interactively #'query-replace from-string to-string nil beg end))
  ;; (define-key evil-normal-state-map (kbd "'") #'evil-query-replace)


  (define-key evil-normal-state-map "^" #'evil-join)

  ;; Insertion
  ;; ============================================================

  (define-key evil-normal-state-map (kbd "o") #'evil-open-below)
  (define-key evil-normal-state-map (kbd "O") #'evil-open-above)
  (define-key evil-normal-state-map (kbd "t") #'evil-insert)
  (define-key evil-normal-state-map (kbd "T") #'evil-append)

  (define-key evil-normal-state-map (kbd "q") #'quoted-insert)

  (defun ds/newline-below ()
    "Open a newline below this line"
    (interactive)
    (save-excursion
      (end-of-line)
      (newline-and-indent)))

  (define-key evil-normal-state-map (kbd "M-RET") #'ds/newline-below)

  ;; Text object selections
  ;; ============================================================

  (define-key evil-normal-state-map (kbd "a") evil-outer-text-objects-map)
  (define-key evil-normal-state-map (kbd "A") evil-inner-text-objects-map)

  ;; paragraphs
  (define-key evil-outer-text-objects-map (kbd "u") #'evil-a-paragraph)
  (define-key evil-outer-text-objects-map (kbd "y") #'evil-a-paragraph)
  (define-key evil-inner-text-objects-map (kbd "u") #'evil-inner-paragraph)
  (define-key evil-inner-text-objects-map (kbd "y") #'evil-inner-paragraph)

  ;; words
  (define-key evil-outer-text-objects-map (kbd "n") #'evil-a-word)
  (define-key evil-outer-text-objects-map (kbd "e") #'evil-a-word)
  (define-key evil-inner-text-objects-map (kbd "n") #'evil-inner-word)
  (define-key evil-inner-text-objects-map (kbd "e") #'evil-inner-word)

  ;; WORDS (i.e. until whitespace)
  (define-key evil-outer-text-objects-map (kbd "N") #'evil-a-WORD)
  (define-key evil-outer-text-objects-map (kbd "E") #'evil-a-WORD)
  (define-key evil-inner-text-objects-map (kbd "N") #'evil-inner-WORD)
  (define-key evil-inner-text-objects-map (kbd "E") #'evil-inner-WORD)

  ;; buffer
  (evil-define-text-object evil-a-buffer (count &optional beg end type)
    "Select entire buffer"
    (evil-range (point-min) (point-max)))
  (define-key evil-outer-text-objects-map (kbd "M-<") #'evil-a-buffer)
  (define-key evil-outer-text-objects-map (kbd "M->") #'evil-a-buffer)
  (define-key evil-outer-text-objects-map (kbd "a") #'evil-a-buffer)


  ;; Other
  ;; ============================================================

  (define-key evil-normal-state-map "." 'evil-repeat)

  (define-key evil-normal-state-map (kbd "z") #'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "Z") #'undo-tree-redo)

  (define-key evil-normal-state-map (kbd "v") #'evil-paste-after)
  (define-key evil-normal-state-map (kbd "V") #'evil-paste-before)
  (define-key evil-normal-state-map (kbd "M-v") #'evil-paste-pop)

  (define-key evil-motion-state-map (kbd "/") #'evil-search-forward)
  (define-key evil-motion-state-map (kbd "f") #'evil-search-forward)
  (define-key evil-motion-state-map (kbd "?") #'evil-search-backward)
  (define-key evil-motion-state-map (kbd "r") #'evil-search-backward)

  ;; ;;; Cursor position jumplist
  ;;   (set-in-all-evil-states-but-insert "(" 'evil-jump-backward)
  ;;   (set-in-all-evil-states-but-insert ")" 'evil-jump-forward)

  ;; Also leave insert state when using the menu
  (define-key evil-insert-state-map (kbd "<menu>") (lambda () (interactive)
                                                     (evil-normal-state)
                                                     (smex)))

  ;; Leader key
  ;; ============================================================

  (defvar ds/evil-leader-map (make-sparse-keymap))
  (define-key evil-normal-state-map (kbd "SPC") ds/evil-leader-map)

  ;; hydras/keymaps
  (define-key ds/evil-leader-map (kbd "p") #'hydra-projectile/body)
  (define-key ds/evil-leader-map (kbd "h") #'hydra-help/body)

  ;; buffer operations
  (define-key ds/evil-leader-map (kbd "s") #'save-buffer)
  (define-key ds/evil-leader-map (kbd "SPC") #'maybe-projectile-find-file)
  (define-key ds/evil-leader-map (kbd "k") #'ido-switch-buffer)
  (define-key ds/evil-leader-map (kbd "q") #'kill-this-buffer)

  (define-key ds/evil-leader-map (kbd "t") #'my-recompile)
  (define-key ds/evil-leader-map (kbd "C-t") #'compile-with-default)

  (define-key ds/evil-leader-map (kbd "n") #'projectile-vc)

  (define-key ds/evil-leader-map (kbd "'") #'query-replace)
  (define-key ds/evil-leader-map (kbd ";") #'eval-expression)
  (define-key ds/evil-leader-map (kbd "$") #'ispell-word)


  ;; Non command settings
  ;; ============================================================

  ;; Use insert state by default in some modes
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (add-to-list 'evil-insert-state-modes 'deft-mode)

  )

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (define-key evil-normal-state-map #')
;;   )
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "nn" 'evil-normal-state)
  )

(use-package evil-matchit
  :ensure t
  :config
  (setq evilmi-may-jump-by-percentage nil)
  (evilmi-init-plugins)
  (define-key evil-normal-state-map (kbd "%") #'evilmi-jump-items))
