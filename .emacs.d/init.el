;; Generic emacs settings, other stuff is in individual files in ./my-files/


;; test with compile command: \emacs --debug-init --batch -u $USER
;; latex lookup nearest equations for ref

;; Use C-\ p as prefix
(set 'projectile-keymap-prefix (kbd "C-\\ p"))


;; install + setup el-get
;; ============================================================

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Install el-get if we don't have it
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/my-recipes")

(el-get 'sync)

;; Load the use-package lib which adds a nice macro for keeping package
;; config all wrapped up together
(require 'ert) ; Need this for now...
(load-library "bind-key")
(load-library "use-package")

;; Some simple, one-line stuff
;; ============================================================
;; (server-start) ;; Start emacs as a server
(line-number-mode 1) ;; Line numbers in mode line
(column-number-mode 1) ;; Column numbers in mode line
(global-linum-mode t) ;; Line numbers on edge of screen
(set 'backup-directory-alist '(("." . ".~"))) ;; Keep backups in .~/
(set 'inhibit-startup-screen t) ;; No startup screen
(set-scroll-bar-mode 'right);; Scroll bar on the right
(global-visual-line-mode 1) ;; Wrap lines at nearest word
(global-subword-mode 1) ;; Treat CamelCase as separate words
(set 'truncate-partial-width-windows nil) ;; Make line wrapping work for
;; multiple frames
(tool-bar-mode -1) ;; Disable toolbar
(menu-bar-mode -1) ;; Disable menu bar
(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no
(show-paren-mode 1) ;; Highlight matching parenthesis
(setq-default fill-column 75) ;; not 80 because when things are later indented
;; by e.g. diff, git log we lose some columns and
;; it gets messy.
(set 'default-abbrev-mode t) ;; Use abbrev mode always
(set 'tags-revert-without-query 1) ;; Autorevert if the tags table has changed

;; Shut up and just open symbolic links
(set 'vc-follow-symlinks t)

;; Allow some disabled commands
(put 'narrow-to-region 'disabled nil)

;; Set the location for bookmarks
(set 'bookmark-default-file "~/.emacs.d/bookmarks")

;; save point in file
(setq-default save-place t)

;; Auto-newlines after { } etc.
;; (add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-newline 1)))

;; Use chrome not firefox to open urls
(set 'browse-url-browser-function 'browse-url-generic)
(set 'browse-url-generic-program "chromium-browser")

;; Draw a line accross the screen instead of ^L for page breaks
(global-page-break-lines-mode t)

;; Show messages on startup, not the stupid scratch buffer
(switch-to-buffer "*Messages*")

;; Set the default font
(set-face-attribute 'default '()
                    :family "DejaVu Sans Mono"
                    :height 98)

;; Highlight long lines
(require 'whitespace)
(set 'whitespace-line-column 80) ;; limit line length
(set 'whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Don't create lockfiles (only safe on single user systems and in ~/ dir
;; technically, but I don't think I'll ever be messing with admin server
;; config files so it's probably fine). Stops emacs spamming up directories
;; with .# files.
(set 'create-lockfiles nil)

;; Show keystrokes in progress (eg show C-\ immediately)
(setq echo-keystrokes 0.1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Sentences do not need double spaces to end (so when moving by sentence
;; use "." to find ends).
(set-default 'sentence-end-double-space nil)


;; Saving
;; ============================================================

;; makes scripts executable automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ;; auto-save in-place
;; (setq auto-save-visited-file-name t)

;; Had to remove this because oomph-lib doesn't stick to no trailing whitespace...,
;; Remove trailing whitespace before save
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
;; oomph-lib safe version is in oomph-lib.el

;; Add a new line at end of file on save if none exists (note: doesn't play
;; nice with scheme).
(setq-default require-final-newline 0)

;; Use frames instead of emacs "windows"
;; ============================================================
(load-file "~/.emacs.d/frames-only-mode/frames-only-mode.el")
(use-package frames-only-mode)

;; Copy/Paste interaction with other X11 applications
;; ============================================================

;; Stop selected regions always overwriting the X primary selection (for
;; example when we briefly select the emacs window and it happens to have
;; some text selected from earlier, without this setting the old text would
;; be put into the primary selection). With this setting only explicit
;; selection with the mouse puts things into the primary selection.
(set 'select-active-regions 'only)

;; after copy Ctrl+c in X11 apps, you can paste by 'yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by 'yank' in emacs
(setq x-select-enable-primary t)

;; Middle click pastes at point not at click position (like in term)
(set 'mouse-yank-at-point 1)


;; Improving Auto complete in minibuffer
;; ============================================================


;; Use ido
(use-package
 ido

 :config
 (progn
   (ido-mode t)

   ;; (for all buffer/file name entry)
   (ido-everywhere)

   ;; Change some keys in ido
   (defun my-ido-keys ()
     (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
     (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
     (define-key ido-completion-map (kbd "C-n") 'ido-select-text)
     (define-key ido-completion-map " " '())
     (define-key ido-completion-map (kbd "S-TAB") 'ido-prev-match)
     ;; Not sure why shift-tab = <backtab> but it works...
     (define-key ido-completion-map (kbd "<backtab>") 'ido-prev-match))

   (add-hook 'ido-setup-hook 'my-ido-keys t)

   ;; Display ido results vertically, rather than horizontally
   (set 'ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                           " [Matched]" " [Not readable]" " [Too big]"
                           " [Confirm]"))

   ;; Enable some fuzzy matching
   (set 'ido-enable-flex-matching t)

   ;; Allow completion (and opening) of buffers that are actually closed.
   (set 'ido-use-virtual-buffers t)

   ;; Not sure if this works yet, supposed to add dir info for duplicate
   ;; virtual buffers.
   (set 'ido-handle-duplicate-virtual-buffers 4)

   ;; increase number of buffers to rememeber
   (set 'recentf-max-saved-items 1000)

   ;; Cycle through commands with tab if we can't complete any further.
   (set 'ido-cannot-complete-command 'ido-next-match)

   ;; Use ido style completion everywhere (separate package)
   ;;(ido-ubiquitous-mode t)

   ;; Buffer selection even if already open elsewhere
   (set 'ido-default-buffer-method 'selected-window)

   ;; Create new buffers without prompting
   (set 'ido-create-new-buffer 'always)

   ;; ??ds Add ignore regex for useless files


   ;; smex: ido based completion for commands
   ;; ============================================================

   ;; Change the main keybinding
   (global-set-key [remap execute-extended-command] 'smex)

   ;; Another key: only list commands relevant to this major mode.
   (global-set-key (kbd "M-|") 'smex-major-mode-commands)

   ;; Tell the prompt that I changed the binding for running commands
   ;; (elsewhere)
   (set 'smex-prompt-string "M-\\: ")

   ;; Put its save file in .emacs.d
   (set 'smex-save-file "~/.emacs.d/smex-items")

   ;; Change some keys in smex itself
   (defun smex-prepare-ido-bindings ()
     (define-key ido-completion-map (kbd "<f1>") 'smex-describe-function)
     (define-key ido-completion-map (kbd "M-.") 'smex-find-function))


   ;; ;; ido for tags
   ;; ;; ============================================================
   ;; (defun my-ido-find-tag ()
   ;;   "Find a tag using ido"
   ;;   (interactive)
   ;;   (tags-completion-table)
   ;;   (let (tag-names)
   ;;     (mapc (lambda (x)
   ;;             (unless (integerp x)
   ;;               (push (prin1-to-string x t) tag-names)))
   ;;           tags-completion-table)
   ;;     (find-tag (ido-completing-read "Tag: " tag-names))))

   ;; ;; From
   ;; ;; http://stackoverflow.com/questions/476887/can-i-get-ido-mode-style-completion-for-searching-tags-in-emacs

   ;; (define-key global-map [remap find-tag] 'my-ido-find-tag)


   ;; ido for help functions
   ;; ============================================================

   ;; There's a whole bunch of code here that I don't really understand, I
   ;; took it from
   ;; https://github.com/tlh/emacs-config/blob/master/tlh-ido.el


   (defmacro aif (test then &rest else)
     `(let ((it ,test))
        (if it ,then ,@else)))

   (defmacro cif (&rest args)
     "Condish `if'"
     (cond ((null args) nil)
           ((null (cdr args)) `,(car args))
           (t `(if ,(car args)
                   ,(cadr args)
                 (cif ,@(cddr args))))))

   (defmacro aand (&rest args)
     (cif (null args)        t
          (null (cdr args))  (car args)
          `(aif ,(car args)  (aand ,@(cdr args)))))

   (defun ido-cache (pred &optional recalc)
     "Create a cache of symbols from `obarray' named after the
predicate PRED used to filter them."
     (let ((cache (intern (concat "ido-cache-" (symbol-name pred)))))
       (when (or recalc (not (boundp cache)))
         (set cache nil)
         (mapatoms (lambda (s)
                     (when (funcall pred s)
                       (push (symbol-name s) (symbol-value cache))))))
       (symbol-value cache)))

   (defun ido-describe-function (&optional at-point)
     "ido replacement for `describe-function'."
     (interactive "P")
     (describe-function
      (intern
       (ido-completing-read
        "Describe function: "
        (ido-cache 'functionp) nil nil
        (aand at-point (function-called-at-point)
             (symbol-name it))))))

   (defun ido-describe-variable (&optional at-point)
     "ido replacement for `describe-variable'."
     (interactive "P")
     (describe-variable
      (intern
       (ido-completing-read
        "Describe variable: "
        (ido-cache 'boundp) nil nil
        (aand at-point (thing-at-point 'symbol) (format "%s" it))))))

   (global-set-key (kbd "<f1> f") 'ido-describe-function)
   (global-set-key (kbd "<f1> v") 'ido-describe-variable)



   ;; Even better fuzzy search for ido
   ;; ============================================================
   (use-package flx-ido
     :config (progn (flx-ido-mode 1)
                    (setq ido-use-faces nil)))


   )
 )



;; Auto complete
;;================================================================
(require 'auto-complete-config)
(require 'fuzzy)
(require 'pos-tip)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Options from
(ac-config-default)

(global-auto-complete-mode t)
(set 'ac-ignore-case nil)
(set 'ac-use-fuzzy t)
(set 'ac-fuzzy-enable t)
(ac-flyspell-workaround)

;; Show quick help (function info display in tooltip)
;; (set 'ac-use-quick-help t)
(set 'ac-delay 0.5) ;; show completions quickly

;; help is too annoying, press f1 to get it
;; ;; (set 'ac-show-menu-immediately-on-auto-complete 1)
;; (set 'ac-quick-help-delay my-ac-delay) ;; show help as soon as it shows
;;                                        ;; completions

;; let me search even while autocomplete is up!
(define-key ac-complete-mode-map (kbd "C-s") nil)

;; Use f1 to show help in a buffer! Handy :) Don't even need to bind
;; anything!


;; Undo tree
;;================================================================
(use-package
 undo-tree
 :config
 (progn
   ;; wipe it's keybinds
   (add-to-list 'minor-mode-map-alist '('undo-tree-mode (make-sparse-keymap)))

   ;; Use it everywhere
   (global-undo-tree-mode))

 )



;; Load my other config files
;; ============================================================

(add-to-list 'load-path "~/.emacs.d/")

;; Load skeletons
(load-file "~/.emacs.d/skeletons.el")

;; Load configs from other files
(load-file "~/.emacs.d/my-files/cpp.el")
(load-file "~/.emacs.d/my-files/colours.el")
(load-file "~/.emacs.d/my-files/latex.el")
(load-file "~/.emacs.d/my-files/oomph-lib.el")
(load-file "~/.emacs.d/my-files/scheme.el")
(load-file "~/.emacs.d/my-files/octave.el")
(load-file "~/.emacs.d/my-files/matlab.el")
(load-file "~/.emacs.d/my-files/org.el")
(load-file "~/.emacs.d/my-files/my-python.el") ;; python-mode is in file called python.el
(load-file "~/.emacs.d/my-files/unicode-entry.el")
(load-file "~/.emacs.d/my-files/haskell.el")


;; Major changes to keybinds
;; Needs to after other file loads so that hooks are in scope
(load-file "~/.emacs.d/my-files/sensible-keys.el")


;; Save command history between sessions
;; ===============================================================

;; Save into a helpfully named file
(set 'savehist-file "~/.emacs.d/savehist")

;; Save other things as well
(set 'savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      compile-command))

;; Enable save history (must be done after changing any variables).
(savehist-mode 1)


;; Compile mode settings
;; ===============================================================

;; Define + active modification to compile that locally sets
;; shell-command-switch to "-ic".
(defadvice compile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

(defadvice recompile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

(defun my-recompile ()
  "Recompile if possible, otherwise compile current buffer."
  (interactive)
  ;; If recompile exists do it, else compile
  (if (fboundp 'recompile) (recompile)
    (compile "make -k")))

(add-hook 'compilation-mode-hook 'my-compilation-mode-keys)
(add-hook 'compilation-shell-mode-hook 'my-compilation-mode-keys)

(defun my-compilation-mode-keys ()
  (local-set-key (kbd "<f5>") 'my-recompile)
  (local-set-key (kbd "<C-f5>") 'compile)
  (local-set-key (kbd "C-`") 'next-error)
  (local-set-key (kbd "C-¬") 'previous-error)
  (local-set-key (kbd "M-`") 'toggle-skip-compilation-warnings))

(defun toggle-skip-compilation-warnings ()
  (interactive)
  (if (equal compilation-skip-threshold 1)
      (set 'compilation-skip-threshold 2)
    (set 'compilation-skip-threshold 1)))

;; scroll compilation buffer to first error
(setq compilation-scroll-output 'first-error)

;; Autosave all modified buffers before compile
(set 'compilation-ask-about-save nil)

;; Handle colours in compile buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(global-set-key (kbd "<f5>") 'my-recompile)
(global-set-key (kbd "C-<f5>") 'compile)

(global-set-key (kbd "C-`") 'next-error)
(global-set-key (kbd "C-¬") 'previous-error)


;; My functions
;; ============================================================
(defun copy-file-name ()
  "Add `buffer-file-name' to the kill ring."
  (interactive)
  (if (not (stringp buffer-file-name))
      (error "Not visiting a file.")
    (kill-new (file-name-nondirectory buffer-file-name))
    ;; Give some visual feedback:
    (message "String \"%s\" saved to kill ring."
             (file-name-nondirectory buffer-file-name))
    buffer-file-name))


(defun copy-file-path ()
  "Add `buffer-file-name' to the kill ring."
  (interactive)
  (if (not (stringp buffer-file-name))
      (error "Not visiting a file.")
    (kill-new buffer-file-name)
    ;; Give some visual feedback:
    (message "String \"%s\" saved to kill ring." buffer-file-name)
    buffer-file-name))


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))


;; ;; what I would like here is something like:
;; (defun just-one-space-dwim
;;   "If there are spaces or tabs to delete then delete them,
;; otherwise delete and newlines AND spaces and tabs"
;;   (interactive)
;;   (if (no-spaces-or-tabs-nearby)
;;       (just-one-space -1)
;;     (just-one-space)))
;; (global-set-key (kbd "M-SPC") 'just-one-space-dwim)
;; ;; Similarly for (kbd "M-\") kill-all-whitespace-dwim
;; ;; instead (for now) just use M-^ : delete-indentation


(defun insert-comment-header ()
  "Insert a line of '=' on the following line and comment it."
  (interactive)
  (save-excursion

    ;; Comment the current line
    (if (not (comment-only-p (point-at-bol) (point-at-eol)))
        (comment-region (point-at-bol) (point-at-eol)))


    ;; Add an underline and comment it
    (newline-and-indent)
    (back-to-indentation) (insert comment-start)
    (just-one-space) ; Some "comment-start"s include a space
    (insert "============================================================")
    (end-of-line) (insert comment-end)
    (newline-and-indent))

  ;; Position point ready to type or continue typing the header
  (end-of-line) (just-one-space))


(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))


(defun un-camelcase-word ()
  (interactive)
  (let ((camel-word (buffer-substring (point)
				      (save-excursion (forward-word) (point)))))
    (kill-word 1)
    (insert-string (un-camelcase-string camel-word))))


(defun generate-org-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch"))
  (org-mode))


(defun clean-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))


;; from https://code.google.com/p/ergoemacs/source/browse/packages/xfrp_find_replace_pairs.el
(defun replace-pairs-region (p1 p2 pairs)
  "Replace multiple PAIRS of find/replace strings in region P1 P2.

PAIRS should be a sequence of pairs [[findStr1 replaceStr1] [findStr2 replaceStr2] …] It can be list or vector, for the elements or the entire argument.

The find strings are not case sensitive. If you want case sensitive, set `case-fold-search' to nil. Like this: (let ((case-fold-search nil)) (replace-pairs-region …))

The replacement are literal and case sensitive.

Once a subsring in the input string is replaced, that part is not changed again.  For example, if the input string is “abcd”, and the pairs are a → c and c → d, then, result is “cbdd”, not “dbdd”. If you simply want repeated replacements, use `replace-pairs-in-string-recursive'.

Same as `replace-pairs-in-string' except does on a region.

Note: the region's text or any string in pairs is assumed to NOT contain any character from Unicode Private Use Area A. That is, U+F0000 to U+FFFFD. And, there are no more than 65534 pairs."
  (let (
        (unicodePriveUseA #xf0000)
        ξi (tempMapPoints '()))
    ;; generate a list of Unicode chars for intermediate replacement. These chars are in  Private Use Area.
    (setq ξi 0)
    (while (< ξi (length pairs))
      (setq tempMapPoints (cons (char-to-string (+ unicodePriveUseA ξi)) tempMapPoints ))
      (setq ξi (1+ ξi))
      )
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)

        ;; replace each find string by corresponding item in tempMapPoints
        (setq ξi 0)
        (while (< ξi (length pairs))
          (goto-char (point-min))
          (while (search-forward (elt (elt pairs ξi) 0) nil t)
            (replace-match (elt tempMapPoints ξi) t t) )
          (setq ξi (1+ ξi))
          )

        ;; replace each tempMapPoints by corresponding replacement string
        (setq ξi 0)
        (while (< ξi (length pairs))
          (goto-char (point-min))
          (while (search-forward (elt tempMapPoints ξi) nil t)
            (replace-match (elt (elt pairs ξi) 1) t t) )
          (setq ξi (1+ ξi)) ) ) ) ) )


(defun change-bracket-pairs (fromType toType)
  "Change bracket pairs from one type to another on text selection or text block.
For example, change all parenthesis () to square brackets [].

When called in lisp program, fromType and toType is a string of a bracket pair. ⁖ \"()\", likewise for toType."
  (interactive
   (let (
         (bracketTypes '("[]" "()" "{}" "“”" "‘’" "〈〉" "《》" "「」" "『』" "【】" "〖〗"))
         )
     (list
      (ido-completing-read "Replace this:" bracketTypes )
      (ido-completing-read "To:" bracketTypes ) ) ) )

  (let* (
         (p1 (if (region-active-p) (region-beginning) (point-min)))
         (p2 (if (region-active-p) (region-end) (point-max)))
         (changePairs (vector
                       (vector (char-to-string (elt fromType 0)) (char-to-string (elt toType 0)))
                       (vector (char-to-string (elt fromType 1)) (char-to-string (elt toType 1)))
                       ))
         )
    (replace-pairs-region p1 p2 changePairs) ) )




(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-b") 'smart-beginning-of-line)
(global-set-key (kbd "C-\\ ;") 'insert-comment-header)
(global-set-key (kbd "C-\\ k") 'generate-org-buffer)



;; Tramp
;; ============================================================

;; set to use ssh
(set 'tramp-default-method "ssh")

;; store backups on my computer
;; (set 'tramp-backup-directory-alist  ??ds


;; Transparency
;; ============================================================

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; No scroll bar is much prettier, lets see if we can live without it
(scroll-bar-mode -1)

;; Set default transparencies:
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(add-to-list 'default-frame-alist '(alpha 90 90))

(set 'edge-background-colour "greasy2")

;; Set all the areas around the edge to be slightly lighter
(set-face-background 'modeline-inactive edge-background-colour)
(set-face-background 'fringe edge-background-colour)
(set-face-background 'linum edge-background-colour)
(set-face-background 'menu edge-background-colour)

;; To get rid of the box around inactive modeline I used custom to set it
;; to the same colour... hacky :(

;; Nice dim line number font colour
(set-face-foreground 'linum "grey20")

;; Set the inactive frame modeline font: Remove the background and the box
;; around it.
(set-face-attribute 'mode-line-inactive nil
		    :background edge-background-colour
		    :box nil)





;; Auto indent pasted code in programming modes
;; ============================================================
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode clojure-mode
						     scheme-mode ruby-mode rspec-mode
						     c-mode c++-mode objc-mode latex-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))


;; Ctags
;; ============================================================

(set 'ctags-command "ctags -e --recurse")
(defun generate-tags (input-command)
  "Generate tags file using ctags."
  (interactive (list (read-string "Ctags command: " ctags-command)))
  (set 'ctags-command input-command)
  (shell-command ctags-command))

(defun regenerate-tags ()
  (interactive)
  "Generate tags file using ctags with previous command."
  (shell-command ctags-command)
  (visit-tags-table "TAGS"))

(global-set-key (kbd "C-.") 'regenerate-tags)

;; Don't do this: it takes far too long to run on oomph-lib
;; ;; Wrap find-tag with code that runs regenerate-tags if tag is not found.
;; (defadvice find-tag (around find-tag activate)
;;   "Rerun etags and reload tags if tag not found and redo find-tag.
;;    If buffer is modified, ask about save before running etags."
;;   (condition-case err
;;       ad-do-it
;;     (error (regenerate-tags)
;;            ad-do-it)))



;; Git
;; ============================================================

;; Use org-mode for git commits
(set 'auto-mode-alist
     (append auto-mode-alist '(("COMMIT_EDITMSG" . markdown-mode))))

;; Show changes vs VC in sidebar
(set 'diff-hl-command-prefix (kbd "C-\\ v"))
(global-diff-hl-mode)


;; Markdown mode
;; ============================================================

(use-package markdown-mode
  :config
  (progn
    ;; run markdown-mode on files ending in .md
    (set 'auto-mode-alist
         (append auto-mode-alist '((".md" . markdown-mode)
                                   (".markdown" . markdown-mode))))
    (defun markdown-mode-keys ()
      (interactive)

      ;; get rid of C-c binds
      (local-set-key (kbd "C-c") nil)

      ;; Compile = preview
      (local-set-key [remap compile] 'markdown-preview)
      (local-set-key [remap my-recompile] 'markdown-preview)

      )


    (add-hook 'markdown-mode-hook 'markdown-mode-keys))
  )

;; Bind goto last change
;; ============================================================

(use-package goto-last-change
             :config
             (progn
               (global-set-key (kbd "C-\\ C-x") 'goto-last-change)))


;; Breadcrumbs
;; ============================================================

(use-package breadcrumb
             :config
             (progn

               ;; Bind some keys
               (global-set-key (kbd "M-b") 'bc-set)
               (global-set-key (kbd "M-B") 'bc-clear)
               (global-set-key [(meta up)] 'bc-previous)
               (global-set-key [(meta down)] 'bc-next)
               (global-set-key [(meta left)] 'bc-local-previous)
               (global-set-key [(meta right)] 'bc-local-next)

               ;; Auto bookmark before isearch
               (add-hook 'isearch-mode-hook 'bc-set)

               ;; Already auto bookmark before tag search and query replace
               ))


;; CUA rectangle support
;; ---------------------
;; Emacs' normal rectangle support is based on interpreting the region
;; between the mark and point as a "virtual rectangle", and using a
;; completely separate set of "rectangle commands" [C-x r ...] on the
;; region to copy, kill, fill a.s.o. the virtual rectangle.
;;
;; cua-mode's superior rectangle support uses a true visual
;; representation of the selected rectangle, i.e. it highlights the
;; actual part of the buffer that is currently selected as part of the
;; rectangle.  Unlike emacs' traditional rectangle commands, the
;; selected rectangle always as straight left and right edges, even
;; when those are in the middle of a TAB character or beyond the end
;; of the current line.  And it does this without actually modifying
;; the buffer contents (it uses display overlays to visualize the
;; virtual dimensions of the rectangle).
;;
;; This means that cua-mode's rectangles are not limited to the actual
;; contents of the buffer, so if the cursor is currently at the end of a
;; short line, you can still extend the rectangle to include more columns
;; of longer lines in the same rectangle.  And you can also have the
;; left edge of a rectangle start in the middle of a TAB character.
;; Sounds strange? Try it!
;;
;; To start a rectangle, use [C-return] and extend it using the normal
;; movement keys (up, down, left, right, home, end, C-home,
;; C-end). Once the rectangle has the desired size, you can cut or
;; copy it using C-x and C-c (or C-w and M-w), and you can
;; subsequently insert it - as a rectangle - using C-v (or C-y).  So
;; the only new command you need to know to work with cua-mode
;; rectangles is C-return!
;;
;; Normally, when you paste a rectangle using C-v (C-y), each line of
;; the rectangle is inserted into the existing lines in the buffer.
;; If overwrite-mode is active when you paste a rectangle, it is
;; inserted as normal (multi-line) text.
;;
;; If you prefer the traditional rectangle marking (i.e. don't want
;; straight edges), [M-p] toggles this for the current rectangle,
;; or you can customize cua-virtual-rectangle-edges.

;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [C-return] cancels the rectangle
;; [C-space] activates the region bounded by the rectangle

;; If you type a normal (self-inserting) character when the rectangle is
;; active, the character is inserted on the "current side" of every line
;; of the rectangle.  The "current side" is the side on which the cursor
;; is currently located. If the rectangle is only 1 column wide,
;; insertion will be performed to the left when the cursor is at the
;; bottom of the rectangle.  So, for example, to comment out an entire
;; paragraph like this one, just place the cursor on the first character
;; of the first line, and enter the following:
;;     C-return M-} ; ;   C-return

;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].

(setq cua-enable-cua-keys nil)
(cua-mode)



;; Registers
;; ============================================================

(use-package list-register)
(global-set-key (kbd "C-\\ r v") 'list-register)
(global-set-key (kbd "C-\\ r s") 'copy-to-register)
(global-set-key (kbd "C-\\ r i") 'insert-register)


;; Mode line
;; ============================================================

;; (defvar mode-line-cleaner-alist
;;   `((auto-complete-mode . "")
;;     (yas/minor-mode . "")
;;     (paredit-mode . "")
;;     (eldoc-mode . "")
;;     (abbrev-mode . "")
;;     (page-break-lines-mode . "")
;;     (visual-line-mode . "")
;;     (global-visual-line-mode . "")
;;     (whitespace-mode "")
;;     (undo-tree-mode "")

;;     ;; Major modes
;;     (lisp-interaction-mode . "λ ")
;;     (hi-lock-mode . "")
;;     (python-mode . "Py ")
;;     (emacs-lisp-mode . "EL ")
;;     (nxhtml-mode . "nx "))
;;   "Alist for `clean-mode-line'.

;; When you add a new element to the alist, keep in mind that you
;; must pass the correct minor/major mode symbol and a string you
;; want to use in the modeline *in lieu of* the original.")


;; (defun clean-mode-line ()
;;   (interactive)
;;   (loop for cleaner in mode-line-cleaner-alist
;;         do (let* ((mode (car cleaner))
;;                   (mode-str (cdr cleaner))
;;                   (old-mode-str (cdr (assq mode minor-mode-alist))))
;;              (when old-mode-str
;;                (setcar old-mode-str mode-str))
;;              ;; major mode
;;              (when (eq mode major-mode)
;;                (setq mode-name mode-str)))))

;; ;; (set 'minor-mode-alist '())
;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Use better unique names for buffers
(use-package uniquify)

;; Pretty modeline
(use-package
 smart-mode-line
 :config
 (progn (sml/setup)

        ;; Shorten some directories to useful stuff
        (add-to-list 'sml/replacer-regexp-list '("^~/oomph-lib/" ":OL:"))
        (add-to-list 'sml/replacer-regexp-list
                     '("^~/oomph-lib/user_drivers/micromagnetics" ":OLMM:"))
        (add-to-list 'sml/replacer-regexp-list '("^~/optoomph/" ":OPTOL:"))
        (add-to-list 'sml/replacer-regexp-list
                     '("^~/optoomph/user_drivers/micromagnetics" ":OPTOLMM:"))
        ))



;; ??ds new file?
;; Use double semi-colon for emacs lisp (default seems to be single).
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";;"
						 comment-end "")))



(global-set-key (kbd "C-\\") ctl-x-map)

;; Projectile
;; ============================================================
(use-package
  projectile
  :pre-load
  (progn
    ;; Use C-\ p as prefix
    (set 'projectile-keymap-prefix (kbd "C-\\ p")))

  :config
  (progn
    ;; Kill C-c keys just in case
    (define-key projectile-mode-map (kbd "C-c") nil)

    ;; Use projectile to open files by default, if available.
    (global-set-key (kbd "C-S-k") (key-binding (kbd "C-k")))

    (defun maybe-projectile-find-file ()
      (interactive)
      (if (projectile-project-p)
          (projectile-find-file)
        (ido-find-file)))
    (global-set-key (kbd "C-k") 'maybe-projectile-find-file)

    ;; Use everywhere
    (projectile-global-mode)
    ))



;; yasnippet?
;; ============================================================

(use-package
  yasnippet
  :config
  (progn

    ;; Load my oomph-lib snippets
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/oomph-snippets")

    ;; kill C-c keys
    (add-hook 'yas-minor-mode-hook
              (lambda ()
                ;; (local-unset-key (kbd "C-c")
                ;; (message "Trying to unset")
                ;; (message (substitute-command-keys "\\{yas-minor-mode-map}"))
                (define-key yas-minor-mode-map (kbd "C-c & C-s") nil)
                (define-key yas-minor-mode-map (kbd "C-c & C-n") nil)
                (define-key yas-minor-mode-map (kbd "C-c & C-v") nil)
                (define-key yas-minor-mode-map (kbd "C-c &") nil)
                (define-key yas-minor-mode-map (kbd "C-c") nil)

                (define-key yas-minor-mode-map (kbd "C-i") nil)
                (define-key yas-minor-mode-map (kbd "TAB") nil)
                (define-key yas-minor-mode-map [tab] nil)


                (set 'yas-fallback-behavior nil)

                ;; (message (substitute-command-keys "\\{yas-minor-mode-map}"))

                ;; For some reason these don't work
                ;; (local-unset-key (kbd "C-c & C-s"))
                ;; (local-set-key (kbd "C-c & C-s") nil)

                ))

    ;; c-mode uses something else on tab, which seems to get messed with by
    ;; yasnippet, remove the extra bindings to prevent this
    (add-hook 'c-mode-common-hook
              (lambda ()
                (define-key c-mode-map [tab] nil)
                (define-key c++-mode-map [tab] nil)
                ;; might need to add more here?
                ))


    ;; Use everywhere
    (yas/global-mode)

    ;; Keys for snippet editing mode
    (add-hook 'snippet-mode-hook
              (lambda ()
                (interactive)
                (local-set-key (kbd "<f5>") 'yas-tryout-snippet)
                (local-set-key (kbd "C-c") nil)
                ))

    (global-set-key (kbd "C-t") 'yas/expand)

    ;; Use minibuffer for yas prompts
    (setq yas-prompt-functions '(yas-ido-prompt))
    )
  )

;; Irony mode (fancy c/c++ autocomplete)
;; ============================================================
(use-package irony

  :disabled

  :config
  (progn
    ;; the ac plugin will be activated in each buffer using irony-mode
    (irony-enable 'ac)             ; hit C-RET to trigger completion

    ;; avoid enabling irony-mode in other modes that inherit from c-mode,
    ;; e.g: php-mode
    (defun irony-mode-if-safe ()
      (interactive)
      (when (member major-mode irony-known-modes)
        (irony-mode 1)))
    (add-hook 'c++-mode-hook 'irony-mode-if-safe)
    (add-hook 'c-mode-hook 'irony-mode-if-safe)

    ;; Kill C-c keys
    (define-key irony-mode-map (kbd "C-c") nil)

    ))


(defun external-shell-in-dir ()
  "Start urxvt in the current file's dir"
  (interactive)
  (start-process "urxvt" nil "urxvt"))
(global-set-key (kbd "C-<f7>") 'external-shell-in-dir)


;; deft (note taking)
;; ============================================================
(use-package deft
  :config
  (progn
    (setq deft-directory "~/Dropbox/notes")
    (setq deft-extension "md")
    (setq deft-text-mode 'markdown-mode)
    (setq deft-use-filename-as-title t)

    ;; Kill C-c keys
    (define-key deft-mode-map (kbd "C-c") 'nil)

    ;; New binds for the useful ones
    (define-key deft-mode-map (kbd "M-RET") 'deft-new-file)

    ;; And move some other binds to fit with my config
    (define-key deft-mode-map (kbd "C-v") 'deft-filter-yank)
    (define-key deft-mode-map (kbd "C-y") 'deft-filter-decrement-word)

    ;; Make a new deft buffer anytime we press f8 (by killing the old one).
    (defun new-clean-deft ()
      (interactive)
      "Close old deft buffer and start a new one"
      (ignore-errors (kill-buffer "*Deft*"))
      (deft)
      )
    (global-set-key [f8] 'new-clean-deft)
    )
  )

;; javascript
;; ============================================================

;; Add parsing of jshint output in compilation mode
(add-to-list 'compilation-error-regexp-alist-alist '(jshint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), " 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'jshint)

;; always clean whitespace in javascript mode
(defun remap-save-clean-whitespace ()
  (interactive)
  (local-set-key [remap save-buffer] 'clean-whitespace-and-save))
(add-hook 'js-mode-hook 'remap-save-clean-whitespace)

;; set up tab key
(add-hook 'js-mode-hook 'set-tab)

;; indent by 2
(set 'js-indent-level 2)


;; Automagically added by customise
;; ============================================================
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cc-other-file-alist (quote (("\\.cc\\'" (".hh" ".h")) ("\\.hh\\'" (".cc" ".C")) ("\\.c\\'" (".h")) ("\\.h\\'" (".cc" ".c" ".C" ".CC" ".cxx" ".cpp")) ("\\.C\\'" (".H" ".hh" ".h")) ("\\.H\\'" (".C" ".CC")) ("\\.CC\\'" (".HH" ".H" ".hh" ".h")) ("\\.HH\\'" (".CC")) ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++")) ("\\.cpp\\'" (".hpp" ".hh" ".h")) ("\\.hpp\\'" (".cpp")) ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx")))))
 '(column-number-mode t)
 '(ff-ignore-include t)
 '(gud-gdb-command-name "gdb -i=mi --args")
 '(htmlize-output-type (quote font))
 '(ido-ignore-buffers (quote ("optimised-oomph-lib" "\\` ")))
 '(indent-tabs-mode nil)
 '(markdown-bold-underscore nil)
 '(org-hide-block-startup t)
 '(org-startup-folded nil)
 '(safe-local-variable-values (quote ((TeX-master . "../poster") (TeX-master . "./main_poster") (TeX-master . "../main_poster") (TeX-master . t) (TeX-master . "main") (TeX-master . "./main"))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
