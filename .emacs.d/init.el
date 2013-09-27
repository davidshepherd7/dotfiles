;; Generic emacs settings, other stuff is in individual files in ./my-files/

;; TODO:

;; actions on block: move to next block afterwards?

;; allow grabbing text from point (like for search) in replace

;; learn + bind bookmarks

;; Enable comment command on empty lines

;; goto last change?

;; Fix stupid copy/paste mouse behvaiour

;; Fix actions on block for last block in file


;; Package management
;; ============================================================

;; ;; Add melpa
;; ;;(if (not (>= emacs-major-version 24))
;; ;;    (error "Requires emacs version 24 or later."))
;; ;;(require 'package)
;; ;;(package-initialize)
;; ;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (defun package-install-if-not-installed (pkg)
;;   "Install a package if it's not already installed."
;;   (if (not (package-installed-p pkg))
;;       (if (assoc pkg package-archive-contents)
;;           (package-install pkg)
;;           (error "Could not find package %s" pkg))
;;     ))

;; Auto update? Possibly not a good idea because things would break

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


;; Some simple, one-line stuff
;; ============================================================
(server-start) ;; Start emacs as a server
(line-number-mode 1) ;; Line numbers in mode line
(column-number-mode 1) ;; Column numbers in mode line
(global-linum-mode t) ;; Line numbers on edge of screen
(setq backup-directory-alist '(("." . ".~"))) ;; Keep backups in .~/
(setq inhibit-startup-screen t) ;; No startup screen
(set-scroll-bar-mode 'right);; Scroll bar on the right
(global-visual-line-mode 1) ;; Wrap lines at nearest word
(setq truncate-partial-width-windows nil) ;; Make line wrapping work for
                                          ;; multiple frames
(tool-bar-mode -1) ;; Disable toolbar
(menu-bar-mode -1) ;; Disable menu bar
(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no
(show-paren-mode 1) ;; Highlight matching parenthesis
(setq-default fill-column 75) ;; not 80 because when things are later indented
                              ;; by e.g. diff, git log we lose some columns and
                              ;; it gets messy.
(setq default-abbrev-mode t) ;; Use abbrev mode always
(setq tags-revert-without-query 1) ;; Autorevert if the tags table has changed

;; Add a new line at end of file on save if none exists (note: doesn't play
;; nice with scheme).
(setq-default require-final-newline 0)

;; Autosave all modified buffers before compile
(setq compilation-ask-about-save nil)

;; Had to remove this because oomph-lib doesn't stick to no trailing whitespace...,
;; Remove trailing whitespace before save
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
;; oomph-lib safe version is in oomph-lib.el

;; Shut up and just open symbolic links
(setq vc-follow-symlinks t)

;; Allow some disabled commands
(put 'narrow-to-region 'disabled nil)

;; Set the location for bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks")

;; Auto-newlines after { } etc.
;; (add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-newline 1)))

;; Use chrome not firefox to open urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Draw a line accross the screen instead of ^L for page breaks
(global-page-break-lines-mode t)

;; Show messages on startup, not the stupid scratch buffer
(switch-to-buffer "*Messages*")

;; Set the default font
(set-face-attribute 'default '()
                    :family "DejaVu Sans Mono"
                    :height 98)

;; Don't do this: it makes everything very slow!
;; ;; Use full .bashrc in evaluating shell commands (i.e. allow the use of
;; ;; aliases in compile commands)
;; (set 'shell-command-switch "-ic")

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

;; Use frames instead of emacs "windows"
;; ============================================================
 ;; Make new frames instead of new windows
(setq pop-up-frames 'graphic-only)

 ;; Disable automatic display of completions buffer because it's a giant pain to
 ;; deal with nicely
(setq completion-auto-help nil)

;; Focus follows mouse off to prevent crazy things happening when I click on
;; e.g. compilation error links.
(setq mouse-autoselect-window nil)
(setq focus-follows-mouse nil)

;; To automatically open a "useful" buffer in new frames xmonads binding for a
;; new frame is set to "emacsclient -c -n -e '(switch-to-buffer nil)'".

;; In py-mode (an improved python mode) we run
;; (py-shell-switch-buffers-on-execute-off)
;; (py-split-windows-on-execute-off)
;; in a hook to prevent crazy window stuff happening.

;; Using ido and smex is a huge help because they replace that stupid
;; completions buffer with a list in the minibuffer itself (the completions
;; buffer is a bit of a pain when using frames--doesn't behave quite
;; right).

;; gdb (gud) does some stupid things with windows, this stops some of it:
(setq gdb-use-separate-io-buffer nil)
(setq gdb-many-windows nil)


;; Improving Auto complete in minibuffer
;; ============================================================

;; Use ido
(require 'ido)
(ido-mode t)
(ido-everywhere) ;; (for all buffer/file name entry)

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
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; sort ido filelist by modificiation time instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
;; (defun ido-sort-mtime ()
;;   (setq ido-temp-list
;;         (sort ido-temp-list
;;               (lambda (a b)
;;                 (time-less-p
;;                  (sixth (file-attributes (concat ido-current-directory b)))
;;                  (sixth (file-attributes (concat ido-current-directory a)))))))
;;   (ido-to-end  ;; move . files to end (again)
;;    (delq nil (mapcar
;;               (lambda (x) (and (char-equal (string-to-char x) ?.) x))
;;               ido-temp-list))))

(defun ido-sort-mtime () '()) ;; actually, disable it because it's so slow

;; Enable some fuzzy matching
(setq ido-enable-flex-matching t)

;; Allow completion (and opening) of buffers that are actually closed.
(setq ido-use-virtual-buffers t)
(setq recentf-max-saved-items 1000) ;; increase number of buffers to rememeber

;; Cycle through commands with tab if we can't complete any further.
(setq ido-cannot-complete-command 'ido-next-match)

;; Use ido style completion everywhere (separate package)
;;(ido-ubiquitous-mode t)

;; Buffer selection even if already open elsewhere
(setq ido-default-buffer-method 'selected-window)

;; Create new buffers without prompting
(setq ido-create-new-buffer 'always)

;; Add ignore regex for useless files?


;; smex: ido based completion for commands


;; Change the main keybinding
(global-set-key [remap execute-extended-command] 'smex)

;; Another key: only list commands relevant to this major mode.
(global-set-key (kbd "M-|") 'smex-major-mode-commands)

;; Tell the prompt that I changed the binding for running commands
;; (elsewhere)
(setq smex-prompt-string "M-\\: ")

;; Put its save file in .emacs.d
(set 'smex-save-file "~/.emacs.d/smex-items")

;; Change some keys in smex itself
(defun smex-prepare-ido-bindings ()
  (define-key ido-completion-map (kbd "<f1>") 'smex-describe-function)
  (define-key ido-completion-map (kbd "M-.") 'smex-find-function))


;; yasnippet
;; ============================================================
(require 'yasnippet)
(yas/global-mode 1)

(add-hook 'snippet-mode-hook
          (lambda () (local-set-key (kbd "<f5>") 'yas/tryout-snippet)))


;; Auto complete
;;================================================================
(require 'auto-complete-config)
(require 'fuzzy)
(require 'pos-tip)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Options from
(ac-config-default)

(global-auto-complete-mode t)
(setq ac-ignore-case nil)
(setq ac-use-fuzzy t)
(setq ac-fuzzy-enable t)
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
(require 'undo-tree)
(add-to-list 'minor-mode-map-alist '('undo-tree-mode (make-sparse-keymap)))
(global-undo-tree-mode)



;; File open keybinds
;; ===============================================================
(global-set-key (kbd "C-<f12>")
                '(lambda () (interactive) (find-file "~/.bashrc")))
(global-set-key (kbd "C-<f11>")
                '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-<f8>")
                '(lambda () (interactive) (find-file "~/.xmonad/xmonad.hs")))
(global-set-key (kbd "C-<f6>") 'ibuffer)


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
(load-file "~/.emacs.d/my-files/my-python.el") ;; python mode is in file called python.el
;; (load-file "~/.emacs.d/my-files/maxima.el")
;; (load-file "~/.emacs.d/my-files/buffer-cycling.el")
(load-file "~/.emacs.d/my-files/unicode-entry.el")
(load-file "~/.emacs.d/my-files/haskell.el")


;; General keybinds
;; ===============================================================

;; Needs to after other file loads so that hooks are in scope

;; Major changes to keybinds
(load-file "~/.emacs.d/my-files/sensible-keys.el")

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

(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

;; (kbd "M-q") to something that auto wraps, indents and arranges spaces..



;; Save command history between sessions
;; ===============================================================

;; Save into a helpfully named file
(setq savehist-file "~/.emacs.d/savehist")

;; Save other things as well
(setq savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      compile-command))

;; Enable save history (must be done after changing any variables).
(savehist-mode 1)


;; Compile mode settings
;; ===============================================================
(add-hook 'compilation-mode-hook 'my-compilation-mode-keys)
(add-hook 'compilation-shell-mode-hook 'my-compilation-mode-keys)
(defun my-compilation-mode-keys ()
  (local-set-key (kbd "<f5>") 'recompile)
  (local-set-key (kbd "C-`") 'next-error)
  (local-set-key (kbd "C-¬") 'previous-error)
  (local-set-key (kbd "M-`") 'toggle-skip-compilation-warnings))

(defun toggle-skip-compilation-warnings ()
  (interactive)
  (if (equal compilation-skip-threshold 1)
      (setq compilation-skip-threshold 2)
    (setq compilation-skip-threshold 1)))

;; "Edit with emacs" (integration with chrome)
;; ============================================================
 (if (and (daemonp) (locate-library "edit-server"))
     (progn
       (require 'edit-server)
       (edit-server-start)))

;; Add current file name to kill ring
;; ============================================================
(defun copy-file-name ()
  "Add `buffer-file-name' to the kill ring."
  (interactive)
  (if (not (stringp buffer-file-name))
      (error "Not visiting a file.")
    (kill-new buffer-file-name)
    ;; Give some visual feedback:
    (message "String \"%s\" saved to kill ring." buffer-file-name)
    buffer-file-name))



;; Smart beginning of line
;; ============================================================
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)


;; Tramp
;; ============================================================

;; set to use ssh
(setq tramp-default-method "ssh")

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
  (setq ctags-command input-command)
  (shell-command ctags-command))

(defun regenerate-tags ()
  (interactive)
  "Generate tags file using ctags with previous command."
  (shell-command ctags-command)
  (visit-tags-table "TAGS"))

(global-set-key (kbd "C-.") 'regenerate-tags)


;; Insert comment header
;; ============================================================

;; Use double semi-colon for emacs lisp (default seems to be single).
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";;"
                                                 comment-end "")))

(defun insert-comment-header ()
  "Insert a line of '=' on the following line and comment it."
  (interactive)
  (save-excursion

    ;; Comment the current line
    (back-to-indentation) (insert comment-start)
    (end-of-line) (insert comment-end)

    ;; Add an underline and comment it
    (newline-and-indent)
    (back-to-indentation) (insert comment-start)
    (just-one-space) ; Some "comment-start"s include a space
    (insert "============================================================")
    (end-of-line) (insert comment-end)
    (newline-and-indent))

  ;; Position point ready to type or continue typing the header
  (end-of-line) (just-one-space))

(global-set-key (kbd "C-\\ ;") 'insert-comment-header)



(defun my-recompile ()
  "Recompile if possible, otherwise compile current buffer."
  (interactive)
  ;; If recompile exists do it, else compile
  (if (fboundp 'recompile) (recompile)
    (compile "make -k")))


;; Set up options for editing git commit messages
;; ============================================================

;; Use org-mode for git commits
(setq auto-mode-alist
      (append auto-mode-alist '(("COMMIT_EDITMSG" . org-mode))))


;; String manipulation functions
;; ============================================================

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


;; ;; Clang based autocomplete
;; ;; ============================================================
;; ;; https://github.com/Golevka/emacs-clang-complete-async


;; ;; Setup clang completion in c-modes
;; (require 'auto-complete-clang-async)
;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
;;   ;; (add-to-list 'ac-sources 'ac-source-clang-async)
;;   (setq ac-sources (ac-source-clang-async))
;;   (ac-clang-launch-completion-process))
;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)


;; Highlight long lines
;; ============================================================
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)


;; make temp buffers easily
;; ============================================================

(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(global-set-key (kbd "C-M-o") 'generate-buffer)


;; Compatatilbity with xcape script
;; ============================================================

;; Breaks other uses of space...
;; ;; Function to do nothing, the (interactive) is important to stop the whole
;; ;; thing being nil, which is not a function.
;; (defun do-nothing () (interactive) nil)

;; ;; Bind" the new xcape space keysym to stop the undefined key spam.
;; (global-set-key (kbd "<key-4660>") 'do-nothing)


;; Automagically added by customise
;; ============================================================
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(gud-gdb-command-name "gdb -i=mi --args")
 '(htmlize-output-type (quote font))
 '(indent-tabs-mode nil)
 '(markdown-bold-underscore nil)
 '(org-hide-block-startup t)
 '(org-startup-folded nil)
 '(safe-local-variable-values (quote ((TeX-master . "../poster") (TeX-master . "./main_poster") (TeX-master . "../main_poster") (TeX-master . t) (TeX-master . "main") (TeX-master . "./main"))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
