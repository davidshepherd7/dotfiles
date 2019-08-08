(use-package evil
  :ensure t
  :config

  ;; Keep the byte compiler happy
  (require 'evil)

  ;; spare keys:
  ;; \,x/
  ;; `!"£$&*_-+=

  ;; TODO

  ;; Case change

  ;; Auto indent insertion

  ;; Replace in region

  ;; Goto line number?

  ;; Improve/learn bindings for isearch integration

  ;; Remove whitespace from text objects?


  ;; Some bindings for these here
  (require 'projectile)
  (require 'hydra)

  ;; Globally enable evil mode
  (evil-mode 1)

  ;; remove all keybindings from most state keymaps
  (setcdr evil-insert-state-map nil)
  (setcdr evil-normal-state-map nil)
  (setcdr evil-motion-state-map nil)

  ;; Why is this a thing? It's crazy...
  (validate-setq evil-move-cursor-back nil)


  ;; Getting to normal state
  ;; ============================================================

  ;; Use evil-change-to-initial-state in places where I want normal-state
  ;; if it's an editing mode, but emacs state for things like magit.

  (validate-setq evil-default-state 'normal)

  ;; keys to get back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "<f35>") 'evil-normal-state)

  ;; Double tap n for normal state
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "ii" #'evil-change-to-initial-state)
    )

  ;; Go to normal state on focus out
  (add-hook 'focus-out-hook #'evil-change-to-initial-state)

  ;; Also leave insert state when using the menu
  (define-key evil-insert-state-map (kbd "<menu>")
    (lambda () (interactive)
      (evil-change-to-initial-state)
      (call-interactively #'counsel-M-x)))


  ;; And when running compile
  (defun evil-change-to-initial-state-ignore-args (&rest r)
    (evil-change-to-initial-state))
  (-each (list #'compile #'recompile #'my-recompile #'projectile-compile-project #'compile-with-default)
    (lambda (fn) (advice-add fn :before #'evil-change-to-initial-state-ignore-args)))

  ;; Cancel company completion when entering normal mode
  (require 'company)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)


  ;; Coloured modeline when in insert mode
  ;; ============================================================
  (defvar ds/mode-line-default-background (face-attribute 'mode-line :background))
  (setq ds/mode-line-evil-insert-background "#002900")
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
  (define-key evil-motion-state-map (kbd "e") #'evil-forward-word-end)
  (define-key evil-motion-state-map (kbd "N") #'evil-backward-WORD-begin)
  (define-key evil-motion-state-map (kbd "E") #'evil-forward-WORD-end)
  (define-key evil-motion-state-map (kbd "C-n") #'evil-backward-char)
  (define-key evil-motion-state-map (kbd "C-e") #'evil-forward-char)

  (define-key evil-motion-state-map (kbd "l") #'evil-end-of-line)

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

  (define-key evil-motion-state-map (kbd "j") #'evil-find-char-to)
  (define-key evil-motion-state-map (kbd "g") #'evil-find-char-to-backward)

  ;; BOL at indentation, unless already there
  (evil-define-motion ds/evil-beginning-of-line (count) (smart-beginning-of-line))
  (define-key evil-motion-state-map (kbd "b") #'ds/evil-beginning-of-line)



  ;; Actions
  ;; ============================================================

  (define-key evil-normal-state-map (kbd "c") #'evil-yank)

  (define-key evil-normal-state-map (kbd "d") #'evil-delete)

  (define-key evil-normal-state-map (kbd "s") #'evil-change)

  (evil-define-operator ds/evil-indent (beg end)
    "Like evil-indent but don't move point"
    :move-point nil
    (indent-region beg end))
  (define-key evil-normal-state-map (kbd "<tab>") #'ds/evil-indent)

  (define-key evil-normal-state-map (kbd "C-<") #'evil-shift-left)
  (define-key evil-normal-state-map (kbd "C->") #'evil-shift-right)

  (evil-define-operator evil-comment (beg end)
    :move-point nil
    (comment-or-uncomment-region beg end))
  (define-key evil-normal-state-map (kbd ";") #'evil-comment)

  (evil-define-operator ds/evil-copy-and-comment (beg end)
    :move-point nil
    (copy-region-as-kill beg end)
    (goto-char end)
    (comment-or-uncomment-region beg end)
    (yank))
  (define-key evil-normal-state-map (kbd ":") #'ds/evil-copy-and-comment)

  (evil-define-operator ds/evil-eval (beg end)
    :move-point nil
    ;; t: print to stdout
    (cond
     ((derived-mode-p 'emacs-lisp-mode) (eval-region beg end t))
     ((derived-mode-p 'sql-mode) (sql-send-region beg end))
     (t (eval-region beg end t))))
  (define-key evil-normal-state-map (kbd "#") #'ds/evil-eval)

  (evil-define-operator evil-mark (beg end)
    (goto-char beg)
    (set-mark-command nil)
    (goto-char end)
    (validate-setq deactivate-mark nil))
  (define-key evil-normal-state-map (kbd "'") #'evil-mark)

  ;; (evil-define-operator evil-query-replace (beg end from-string to-string)
  ;;   (call-interactively #'query-replace from-string to-string nil beg end))
  ;; (define-key evil-normal-state-map (kbd "'") #'evil-query-replace)

  (evil-define-operator evil-destroy (beg end type register yank-handler)
    "Delete and don't copy to kill-ring"
    (evil-delete beg end type ?_ yank-handler))

  (evil-define-operator evil-destroy-replace (beg end type register yank-handler)
    "Replace something with the clipboard"
    (evil-destroy beg end type register yank-handler)
    (evil-paste-before 1 register))
  (define-key evil-normal-state-map (kbd "C-v") #'evil-destroy-replace)

  (define-key evil-normal-state-map "^" #'evil-join)

  (use-package smartparens
    :ensure t
    :config

    ;; move by sexp
    (evil-define-motion ds/evil-sp-previous-sexp (count)
      ;; Use negative arg and next-sexp so that point always lands at the start
      (sp-next-sexp (* -1 (or count 1))))
    (define-key evil-motion-state-map (kbd "I") #'ds/evil-sp-previous-sexp)

    (evil-define-motion ds/evil-sp-next-sexp (count) (sp-next-sexp count))
    (define-key evil-motion-state-map (kbd "H") #'ds/evil-sp-next-sexp)

    (evil-define-motion ds/evil-sp-down-sexp (count) (sp-down-sexp count))
    (define-key evil-motion-state-map (kbd "K") #'ds/evil-sp-down-sexp)

    ;; slurp and barf
    (define-key evil-normal-state-map (kbd "k") #'sp-slurp-hybrid-sexp)
    (define-key evil-normal-state-map (kbd "m") #'sp-forward-barf-sexp)
    (define-key evil-normal-state-map (kbd "M") #'sp-backward-barf-sexp)

    ;; Not really evil-mode, but uses smartparens:
    (global-set-key (kbd "C-M-t") #'sp-transpose-hybrid-sexp)
    )

  ;; Insertion
  ;; ============================================================

  (define-key evil-normal-state-map (kbd "o") #'evil-open-below)
  (define-key evil-normal-state-map (kbd "O") #'evil-open-above)
  (define-key evil-normal-state-map (kbd "t") #'evil-insert)
  (define-key evil-normal-state-map (kbd "T") #'evil-append)

  (define-key evil-normal-state-map (kbd "B") (lambda () (interactive) (ds/evil-beginning-of-line) (evil-insert 1)))
  (define-key evil-normal-state-map (kbd "L") (lambda () (interactive) (evil-end-of-line) (evil-append 1)))

  (define-key evil-normal-state-map (kbd "q") #'quoted-insert)

  (defun ds/newline-below ()
    "Open a newline below this line"
    (interactive)
    (save-excursion
      (end-of-line)
      (newline-and-indent)))

  (define-key evil-normal-state-map (kbd "M-RET") #'ds/newline-below)

  (defun ds/blank-line-p ()
    (string-blank-p (thing-at-point 'line)))

  (defun ds/evil-insert-new-comment ()
    (interactive)
    (beginning-of-line)
    (unless (ds/blank-line-p)
      (save-excursion (insert "\n")))
    (ds/evil-insert-new-comment-here))

  (defun ds/evil-insert-new-comment-here ()
    (interactive)
    (insert comment-start)
    (insert " ")
    (save-excursion
      (insert comment-end)
      (indent-for-tab-command))
    (evil-insert-state))

  (define-key evil-normal-state-map (kbd "C-;") #'ds/evil-insert-new-comment)

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

  ;; lines
  (evil-define-text-object evil-i-line (count &optional beg end type)
    (evil-range (save-excursion (back-to-indentation) (point))
                (save-excursion (end-of-line) (point))))
  (define-key evil-inner-text-objects-map (kbd "l") #'evil-i-line)
  (define-key evil-inner-text-objects-map (kbd "b") #'evil-i-line)
  (evil-define-text-object evil-a-line (count &optional beg end type)
    (evil-range (point-at-bol) (point-at-eol)))
  (define-key evil-outer-text-objects-map (kbd "l") #'evil-a-line)
  (define-key evil-outer-text-objects-map (kbd "b") #'evil-a-line)


  ;; buffer
  (evil-define-text-object evil-a-buffer (count &optional beg end type)
    "Select entire buffer"
    (evil-range (point-min) (point-max)))
  (define-key evil-outer-text-objects-map (kbd "M-<") #'evil-a-buffer)
  (define-key evil-outer-text-objects-map (kbd "M->") #'evil-a-buffer)
  (define-key evil-outer-text-objects-map (kbd "a") #'evil-a-buffer)

  ;; functions, ripped off from mark-defun
  (evil-define-text-object ds/evil-a-defun (count &optional beg end type)
    "Select entire buffer"
    (save-mark-and-excursion
     (mark-defun)
     (evil-range (region-beginning) (region-end))))
  (define-key evil-outer-text-objects-map (kbd "f") #'ds/evil-a-defun)


  (use-package evil-args
    :ensure t
    :config
    (define-key evil-outer-text-objects-map (kbd ",") #'evil-outer-arg)
    (define-key evil-inner-text-objects-map (kbd ",") #'evil-inner-arg)
    )

  ;; Other
  ;; ============================================================

  (define-key evil-normal-state-map "." 'evil-repeat)

  (define-key evil-normal-state-map (kbd "z") #'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "Z") #'undo-tree-redo)

  (define-key evil-normal-state-map (kbd "v") #'evil-paste-after)
  (define-key evil-normal-state-map (kbd "V") #'evil-paste-before)
  (define-key evil-normal-state-map (kbd "M-v") #'evil-paste-pop)

  (define-key evil-motion-state-map (kbd "/") #'evil-search-forward)
  (define-key evil-motion-state-map (kbd "?") #'evil-search-backward)
  (define-key evil-motion-state-map (kbd "r") #'evil-search-backward)

  ;; ;;; Cursor position jumplist
  ;;   (set-in-all-evil-states-but-insert "(" 'evil-jump-backward)
  ;;   (set-in-all-evil-states-but-insert ")" 'evil-jump-forward)


  ;; Leader key
  ;; ============================================================

  (defvar ds/evil-leader-map (make-sparse-keymap))
  (define-key evil-normal-state-map (kbd "SPC") ds/evil-leader-map)

  (require 'magit-status)
  (define-key magit-mode-map (kbd "SPC") ds/evil-leader-map)

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
  (add-hook 'emerge-startup-hook #'evil-insert-state)
  (add-to-list 'evil-insert-state-modes 'deft-mode)
  (add-to-list 'evil-insert-state-modes 'makey-key-mode)
  (add-to-list 'evil-insert-state-modes 'git-rebase-mode)
  (add-to-list 'evil-insert-state-modes 'diff-mode)
  (add-to-list 'evil-insert-state-modes 'special-mode)
  (add-to-list 'evil-insert-state-modes 'anki-mode)
  (add-to-list 'evil-insert-state-modes 'anki-mode-menu-mode)
  (add-to-list 'evil-insert-state-modes 'xref--xref-buffer-mode)


  ;; Square brackets
  ;; ============================================================

  ;; Why the hell is this not already a thing?

  (evil-define-motion evil-previous-open-square-bracket (count)
    "Go to [count] previous unmatched '['."
    :type exclusive
    (evil-up-paren ?\[ ?\] (- (or count 1))))

  (evil-define-motion evil-previous-close-square-bracket (count)
    "go to [count] next unmatched ']'."
    :type exclusive
    (forward-char)
    (evil-up-paren ?\[ ?\] (or count 1))
    (backward-char))

  (define-key evil-normal-state-map (kbd "[") #'evil-previous-open-square-bracket)
  (define-key evil-normal-state-map (kbd "]") #'evil-previous-close-square-bracket)

  (evil-define-motion evil-previous-open-angle-bracket (count)
    "Go to [count] previous unmatched '<'."
    :type exclusive
    (evil-up-paren ?< ?> (- (or count 1))))

  (evil-define-motion evil-previous-close-angle-bracket (count)
    "go to [count] next unmatched '>'."
    :type exclusive
    (forward-char)
    (evil-up-paren ?< ?> (or count 1))
    (backward-char))

  (define-key evil-normal-state-map (kbd "<") #'evil-previous-open-angle-bracket)
  (define-key evil-normal-state-map (kbd ">") #'evil-previous-close-angle-bracket)

  (evil-define-motion ds/evil-end-of-defun (count) (end-of-defun))
  (define-key evil-motion-state-map (kbd "f") #'ds/evil-end-of-defun)
  (evil-define-motion ds/evil-beginning-of-defun (count) (beginning-of-defun))
  (define-key evil-motion-state-map (kbd "F") #'ds/evil-beginning-of-defun)

  )

(use-package evil-surround
  :ensure t
  :config
  ;; Disable default keys
  (validate-setq evil-surround-mode-map (make-sparse-keymap))

  ;; use w instead
  (define-key evil-normal-state-map (kbd "w") #'evil-surround-edit)
  )


(use-package evil-matchit
  :ensure t
  :config
  (validate-setq evilmi-may-jump-by-percentage nil)
  (evilmi-init-plugins)
  (define-key evil-normal-state-map (kbd "%") #'evilmi-jump-items))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)

  (validate-setq evil-goggles-duration 0.1))


;; Just for the comment text objects
(use-package evil-nerd-commenter
  :ensure t
  :config
  (define-key evil-inner-text-objects-map (kbd ";") #'evilnc-inner-comment)
  (define-key evil-outer-text-objects-map (kbd ";") #'evilnc-outer-commenter))
