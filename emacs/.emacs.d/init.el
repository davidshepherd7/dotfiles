;; Generic emacs settings, other stuff is in individual files in ./lisp/

;; test with compile command: \emacs --debug-init --batch -u $USER


;; Use C-\ p as prefix
(set 'projectile-keymap-prefix (kbd "C-\\ p"))
(global-set-key (kbd "C-\\") ctl-x-map)



(defvar ds/emacs-up-to-date? (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
  "Are we using my prefered emacs version or newer?")

;; TODO:

;; Bootstrap use-package

;; use-package s and dash

;; C-x from discover mode

;; Set up "package" package manager etc.
;; ============================================================

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)


;; Some packages I want to play with, so store version controlled copies
;; outside of package.el's control. Recursively add the relevant dirs to
;; the load path.
(let ((base "~/.emacs.d/vc-packages"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))


;; Load the use-package lib which adds a nice macro for keeping package
;; config all wrapped up together
(require 'ert) ; Need this for now...
(load-library "bind-key")
(load-library "use-package")

;; add some packages for improving package.el
(use-package package-utils :ensure t)
(use-package paradox :ensure t)


(setq global-edebug-prefix "\C-\\X")
(setq edebug-inhibit-emacs-lisp-mode-bindings t)
(require 'edebug)


;; Pretty colours
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'shepherd t)
(load-theme 'adwaita t)


(defun starting-comment-p ()
  "Are we starting to insert a c/java mode comment?"
  (interactive)
  (and (derived-mode-p 'cpp-mode 'java-mode 'c-mode)
       (looking-back "/")))

(use-package aggressive-indent
  :ensure t
  :config
  ;; Enable in modes where it's safe
  (mapcar (lambda (hook) (add-hook hook #'aggressive-indent-mode))
          (list 'c-mode-hook 'c++-mode-hook 'emacs-lisp-mode-hook
                'java-mode-hook 'sh-mode-hook 'ess-mode-hook))

  (define-key aggressive-indent-mode-map (kbd "C-c") nil)

  (add-hook 'c-mode-common-hook
            (lambda () (define-key c-mode-map (kbd "C-d") nil)))

  ;; Don't indent when entering comment start character (only for
  ;; c/c++/java at the moment). The extra () is because
  ;; aggressive-indent-dont-indent-if is a list of *expressions* not
  ;; functions.
  (add-to-list 'aggressive-indent-dont-indent-if '(starting-comment-p))

  )


;; Some simple, one-line stuff
;; ============================================================
;; (server-start) ;; Start emacs as a server
(line-number-mode 1) ;; Line numbers in mode line
(column-number-mode 1) ;; Column numbers in mode line
(global-linum-mode t) ;; Line numbers on edge of screen
(set 'backup-directory-alist '(("." . ".~"))) ;; Keep backups in .~/
(set 'inhibit-startup-screen t) ;; No startup screen
(scroll-bar-mode -1);; No scroll bar
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
(set 'browse-url-generic-program "firefox")

;; Draw a line accross the screen instead of ^L for page breaks
(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode t))

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


;; Always revert buffer to file
(global-auto-revert-mode)

;; Automatically add newlines after certain characters (e.g. '{')
(electric-layout-mode)

;; Make the rules for electric layout local to buffers (so that different
;; modes don't interfere)
(make-variable-buffer-local 'electric-layout-rules)

;; Automatically add closing character where appropriate
(electric-pair-mode)

;; show number of matches when searching
(use-package anzu
  :ensure t
  :config (global-anzu-mode))

;; Save point location in files even between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; Saving
;; ============================================================

;; makes scripts executable automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ;; auto-save in-place
;; (setq auto-save-visited-file-name t)

;; Add a new line at end of file on save if none exists (note: doesn't play
;; nice with scheme).
(setq-default require-final-newline 0)


;; Create non-existant directories automatically
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)


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

;; Add things copy pasted from other programs to the kill
(set 'save-interprogram-paste-before-kill t)


;; Auto complete
;;================================================================
(use-package fuzzy :ensure t :disabled)
(use-package pos-tip :ensure t :disabled)
(use-package auto-complete
  :disabled
  :ensure t
  :config
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
  )

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (global-set-key (kbd "<C-tab>") #'company-complete)
  )

;; Undo tree
;;================================================================
(use-package undo-tree
  :ensure t
  :demand
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode)

  ;; clean out the undo-tree keymap entry in the keymap list
  (let ((item (assoc 'undo-tree-mode minor-mode-map-alist)))
    (setf (cdr item) (make-sparse-keymap)))

  ;; For consistency make the keymap itself empty as well
  (setf undo-tree-map (make-sparse-keymap))

  )



;; Load my other config files
;; ============================================================

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Load skeletons
(load-file "~/.emacs.d/skeletons.el")

;; Load configs from other files
(load-file "~/.emacs.d/lisp/ds-cpp.el")
(load-file "~/.emacs.d/lisp/ds-latex.el")
(load-file "~/.emacs.d/lisp/ds-oomph-lib.el")
(load-file "~/.emacs.d/lisp/ds-scheme.el")
(load-file "~/.emacs.d/lisp/ds-octave.el")
(load-file "~/.emacs.d/lisp/ds-matlab.el")
(load-file "~/.emacs.d/lisp/ds-org.el")
(load-file "~/.emacs.d/lisp/ds-python.el")
(load-file "~/.emacs.d/lisp/ds-unicode-entry.el")
(load-file "~/.emacs.d/lisp/ds-haskell.el")
(load-file "~/.emacs.d/lisp/ds-elisp.el")
(load-file "~/.emacs.d/lisp/ds-java.el")


;; Helm or ido...
(load-file "~/.emacs.d/lisp/ds-ido.el")
(load-file "~/.emacs.d/lisp/ds-helm.el")


;; Major changes to keybinds
;; Needs to after other file loads so that hooks are in scope
(load-file "~/.emacs.d/lisp/ds-sensible-keys.el")


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

;; Modification to compile/recompile that locally sets shell-command-switch
;; to "-ic", in order to make sure that the path, aliases and functions are
;; all set correctly.
(defadvice compile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))
(defadvice recompile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

(defun buffer-has-hashbang ()
  (save-excursion (goto-char (point-min))
                  (looking-at-p "#!")))

(defun compile-default-command ()
  (interactive)
  (cond
   ;; Files with hashbang should be run
   ((and (buffer-has-hashbang)
         (buffer-file-name))
    (concat "./" (file-name-nondirectory (buffer-file-name))))

   ;; emacs lisp should be tested by running a new emacs instance in batch
   ;; mode
   ((derived-mode-p 'emacs-lisp-mode)
    "\\emacs --debug-init --batch -u $USER")

   ((derived-mode-p 'tex-mode)
    (concat "latexmk -C && latexmk -pdf" " " (file-name-nondirectory (buffer-file-name))))

   ;; make is probably a good default for anything else
   (t "make")))

(defun compile-with-default ()
  (interactive)
  ;; Run compile with a specially chosen default command
  (set 'backup-compile-command compile-command)
  (set 'compile-command (compile-default-command))
  (condition-case nil (call-interactively #'compile)
    ;; If we quit then revert compile command (so that it isn't in the
    ;; history).
    (quit (set 'compile-command backup-compile-command))))

(defun my-recompile ()
  "Recompile if possible, otherwise compile current buffer."
  (interactive)
  ;; If recompile exists do it, else compile
  (if (fboundp 'recompile) (recompile)
    (call-interactively #'compile)))

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

(set 'compilation-always-kill t)

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
(global-set-key (kbd "C-<f5>") 'compile-with-default)

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

(require 's)

(defun insert-comment-header ()
  "Insert a line of '=' on the following line and comment it."
  (interactive)
  (save-excursion

    ;; Comment the current line if necessary (not a comment, or empty)
    (when (or (not (comment-only-p (point-at-bol) (point-at-eol)))
              (string-match-p "^[ ]*$" (thing-at-point 'line)))
      (indent-for-tab-command)
      (back-to-indentation)
      (insert comment-start) (just-one-space)
      (end-of-line) (insert comment-end))

    ;; Add an underline and comment it
    (end-of-line)
    (newline-and-indent)
    (back-to-indentation) (insert comment-start)
    (just-one-space) ; Some "comment-start"s include a space
    (insert (s-repeat 60 "="))
    (end-of-line) (insert comment-end)
    (newline-and-indent))

  ;; Position point ready to type or continue typing the header
  (end-of-line))


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


(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-b") 'smart-beginning-of-line)
(global-set-key (kbd "C-\\ ;") 'insert-comment-header)
(global-set-key (kbd "C-\\ k") 'generate-org-buffer)

;; from emacs wiki
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )



;; Tramp
;; ============================================================

;; set to use ssh
(set 'tramp-default-method "ssh")

;; store backups on my computer
;; (set 'tramp-backup-directory-alist  ??ds


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


;; Git
;; ============================================================

(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :ensure t)

;; Use markdown mode for git commits (github displays it nicely)
(set 'auto-mode-alist
     (append auto-mode-alist '(("COMMIT_EDITMSG" . markdown-mode))))

;; Show changes vs VC in sidebar
(use-package diff-hl
  :ensure t
  :init (set 'diff-hl-command-prefix (kbd "C-\\ v"))
  :config (global-diff-hl-mode))


;; Markdown mode
;; ============================================================

(use-package markdown-mode
  :ensure t
  :config
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


  (add-hook 'markdown-mode-hook 'markdown-mode-keys)
  )

;; Bind goto last change
;; ============================================================

(use-package goto-last-change
  :ensure t
  :config (global-set-key (kbd "C-\\ C-x") 'goto-last-change))


;; Breadcrumbs
;; ============================================================

;; built in, don't need to use-package

;; ;; Bind some keys
;; (global-set-key (kbd "M-b") 'bc-set)
;; (global-set-key (kbd "M-B") 'bc-clear)
;; (global-set-key [(meta up)] 'bc-previous)
;; (global-set-key [(meta down)] 'bc-next)
;; (global-set-key [(meta left)] 'bc-local-previous)
;; (global-set-key [(meta right)] 'bc-local-next)

;; ;; Auto bookmark before isearch
;; (add-hook 'isearch-mode-hook 'bc-set)

;; Already auto bookmark before tag search and query replace



;; Registers
;; ============================================================

(use-package list-register
  :ensure t
  :config
  (global-set-key (kbd "C-\\ r v") 'list-register)
  (global-set-key (kbd "C-\\ r s") 'copy-to-register)
  (global-set-key (kbd "C-\\ r i") 'insert-register))


;; Mode line
;; ============================================================

;; Pretty modeline
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)

  (sml/setup)

  (sml/apply-theme 'automatic)

  ;; Shorten some directories to useful stuff
  (add-to-list 'sml/replacer-regexp-list '("^~/oomph-lib/" ":OL:"))
  (add-to-list 'sml/replacer-regexp-list
               '("^~/oomph-lib/user_drivers/micromagnetics" ":OLMM:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/optoomph/" ":OPTOL:"))
  (add-to-list 'sml/replacer-regexp-list
               '("^~/optoomph/user_drivers/micromagnetics" ":OPTOLMM:"))
  )


;; Projectile
;; ============================================================
(use-package projectile
  :ensure t
  :init
  ;; Use C-\ p as prefix
  (set 'projectile-keymap-prefix (kbd "C-\\ p"))

  :config
  ;; Kill C-c keys just in case
  (define-key projectile-mode-map (kbd "C-c") nil)

  ;; Use projectile to open files by default, if available.
  (defun maybe-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (projectile-find-file)
      (call-interactively #'find-file)))
  (global-set-key (kbd "C-k") 'maybe-projectile-find-file)

  ;; Use everywhere
  (projectile-global-mode)

  (set 'projectile-use-git-grep t)

  (global-set-key (kbd "<f8>") #'projectile-grep)

  (set 'projectile-tags-command "ctags-exuberant -eR -f \"%s\" %s")
  )



;; yasnippet?
;; ============================================================

(use-package yasnippet
  :ensure t
  :config

  ;; Load my oomph-lib snippets
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/oomph-snippets" t)

  ;; and my other snippets
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets" t)


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
              (local-set-key (kbd "<f6>") 'yas-load-snippet-buffer)
              ))

  (global-set-key (kbd "C-t") 'yas/expand)

  ;; Use minibuffer for yas prompts
  (setq yas-prompt-functions '(yas-ido-prompt))


  ;; Faster snippet creation:

  (defun ds/yas-snippet-dir ()
    "Get the correct snippet dir for this buffer's mode

Wrapper function to handle all yasnippets weirdness."
    (yas--make-directory-maybe (first (yas--guess-snippet-directories))))

  (defun ds/read-with-default-word-at-point (prompt)
    ;; All this crap is needed to use the word at point as the default value.
    (list (read-string (format "%s (default %s): " prompt (thing-at-point 'word))
                       nil nil (thing-at-point 'word))))

  (defun ds/new-yasnippet (snippet-name)
    "Quickly create a new snippet.

   In the right dir; with filename and snippet name already set
   up; with minimal prompts."
    (interactive (list (thing-at-point 'word)))
    (let ((snippet-file-name (s-concat (ds/yas-snippet-dir) "/" snippet-name)))

      (if (file-exists-p (s-concat (ds/yas-snippet-dir) "/" snippet-name))
          (find-file snippet-file-name)

        ;; Else create it

        ;; Save before expanding the snippet so that we can get the buffer name
        ;; from inside the snippet.
        (yas-new-snippet t)
        (write-file snippet-file-name t)
        (yas-expand-snippet yas-new-snippet-default))))

  (global-set-key (kbd "C-S-T") 'ds/new-yasnippet)

  ;; Don't add a binding by default in new snippets
  (set 'yas-new-snippet-default "\
# -*- mode: snippet; require-final-newline: nil -*-
# key: `(file-name-nondirectory buffer-file-name)`
# --
$0")

  )

;; Irony mode (fancy c/c++ autocomplete)
;; ============================================================
(use-package irony

  :disabled

  :config
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

  )


(defun external-shell-in-dir ()
  "Start urxvt in the current file's dir"
  (interactive)
  (start-process "urxvt" nil "urxvt"))
(global-set-key (kbd "C-<f7>") 'external-shell-in-dir)


;; deft (note taking)
;; ============================================================
(use-package deft
  :ensure t
  :config
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

  ;; Make a new deft buffer (by killing the old one). Called from xmonad.
  (defun new-clean-deft ()
    (interactive)
    "Close old deft buffer and start a new one"
    (ignore-errors (kill-buffer "*Deft*"))
    (deft)
    ))

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


;; Shell mode
;; ============================================================

(add-hook 'shell-mode-hook 'set-tab)
(add-hook 'shell-mode-hook (lambda ()
			     (set 'sh-basic-offset 2)
			     (set 'sh-indentation 2)))


;; C mode
;; ============================================================

(define-key c-mode-map (kbd "C-c") nil)
(add-hook 'c-mode-hook #'set-tab)




;; Ace jump mode
(use-package ace-jump-mode
  :ensure t
  :config
  (set 'ace-jump-mode-case-fold t)

  ;; favour home row keys
  (let ((first-list  '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h)))
    (set 'ace-jump-mode-move-keys
         (nconc first-list
                (-difference (loop for i from ?a to ?z collect i) first-list)
                (loop for i from ?A to ?Z collect i))))

  (set 'ace-jump-mode-scope 'window)
  (global-set-key (kbd "C-p") 'ace-jump-mode))


(use-package expand-region
  :ensure t
  :bind ("C-a" . er/expand-region))

;; (use-package ggtags
;;   :disabled
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook #'ggtags-mode)
;;   (define-key ggtags-mode-map (kbd "C-c") nil)
;;   (define-key ggtags-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
;;   (set 'ggtags-mode-line-project-name nil)


;;   (define-key ggtags-mode-map (kbd "M-<") nil)
;;   (define-key ggtags-mode-map (kbd "M->") nil))

;; Better help commands
;; ============================================================

;; Show source code for things
(define-key 'help-command (kbd "F") 'find-function)
(define-key 'help-command (kbd "K") 'find-function-on-key)
(define-key 'help-command (kbd "V") 'find-variable)


;; Clean up whitespace
;; ============================================================

;; has to go near the end because we need some project/language specific
;; functions.

(defun preserve-trailing-whitespace-p ()
  (interactive)
  (or
   ;; makefile mode doesn't play well with this
   (string= major-mode "makefile-mode")

   ;; oomph-lib doesn't stick to no trailing whitespace :(
   (is-oomph-code)

   ;; Don't do it in non-programming modes (just to be safe)
   (not (or (derived-mode-p 'prog-mode)
	    (eq major-mode 'ess-mode)
            (eq major-mode 'feature-mode)))))

(defun maybe-delete-trailing-whitespace ()
  (when (not (preserve-trailing-whitespace-p))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)

(add-to-list 'load-path "~/.emacs.d/aggressive-fill-paragraph-mode")
(load-library "aggressive-fill-paragraph")
(use-package aggressive-fill-paragraph
  :config
  ;; Enable in modes where it works nicely
  (mapcar (lambda (hook) (add-hook hook #'aggressive-fill-paragraph-mode))
          (list 'c-mode-hook
                'c++-mode-hook
                'emacs-lisp-mode-hook
                'java-mode-hook
                'sh-mode-hook
                'python-mode-hook
                'org-mode-hook
                'ess-mode-hook))
  )


;; For some stupid reason ess doesn't define this autoload, or this
;; file association!
(autoload 'R-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

(use-package ess
  :ensure t
  :config
  (define-key ess-mode-map (kbd "C-c") nil)
  (define-key ess-mode-map (kbd "C-x") nil)

  ;; ess-mode tries to do some stupid stuff with '_' and ',',
  ;; disable this.
  (define-key ess-mode-map (kbd "_") nil)
  (define-key ess-mode-map (kbd ",") nil)

  (add-hook 'ess-roxy-mode-hook
            (lambda () (define-key ess-roxy-mode-map (kbd "C-c") nil)))

  (add-hook 'ess-mode-hook
            (lambda() (ess-set-style 'C++ 'quiet)
              (set 'ess-arg-function-offset t)))

  ;; auto newline after '{'
  (add-hook 'ess-mode-hook
            (lambda()
              (add-to-list 'electric-layout-rules '( ?\{ .  after))
              (add-to-list 'electric-layout-rules '( ?\} .  before))
              ))

  ;; Use rtags to make the tags file
  (add-hook 'ess-mode-hook
            (lambda()
              (make-local-variable 'projectile-tags-command)
              (set 'projectile-tags-command
                   "R -e 'rtags(recursive=TRUE,ofile=\"%s\")'")))
  )


(use-package discover
  :config (global-discover-mode)
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/electric-spacing")
(load-library "electric-spacing")
(use-package electric-spacing
  :config
  ;; Enable in some modes
  (mapcar (lambda (hook) (add-hook hook #'electric-spacing-mode))
          '(python-mode-hook
            ess-mode-hook
            c-mode-hook
            c++-mode-hook
            java-mode-hook))
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(use-package feature-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
  (define-key feature-mode-map (kbd "C-c") nil)
  (define-key orgtbl-mode-map (kbd "C-c") nil)
  (add-hook 'feature-mode-hook #'set-tab))

(use-package hydra
  :ensure t
  :config
  ;; A more readable blue
  (set-face-attribute 'hydra-face-blue nil :foreground "RoyalBlue1")

  (define-key help-map (kbd "n") #'man)

  (defhydra hydra-help (:exit t)
    ;; Better to exit after any command because otherwise helm gets in a
    ;; mess, set hint to nil: written out manually.

    "
  Describe        ^^Keys                    ^^Search                    ^^Documentation
  ---------------------------------------------------------------------------------------
  _f_unction        _k_eybinding              _a_propros                  _i_nfo
  _p_ackage         _w_here-is                _d_oc strings               _n_: man
  _m_ode            _b_: show all bindings    _s_: info by symbol         _h_elm-dash
  _v_ariable

  "
    ;; Boring help commands...
    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")


    ;; Documentation
    ("i" info nil)
    ("n" helm-man-woman nil)
    ("h" helm-dash)

    ;; Keybinds
    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("y" describe-syntax nil)

    ;; quit
    ("q" help-quit "quit"))
  (global-set-key (kbd "<f1>") #'hydra-help/body)
  (global-set-key (kbd "C-<f1>") help-map)
  )


;; (setq helm-dash-common-docsets '("Python 3"))

(add-to-list 'load-path "~/code/helm-dash")
(use-package helm-dash
  :config

  ;; base docsets
  (set 'ds/docsets '("Bash" "Emacs Lisp" "Markdown"
                     "Python 3" "SciPy" "NumPy"
                     "C" "C++"
                     "Haskell" "R"))

  ;; user contrib docsets
  (set 'ds/contrib-docsets '("SymPy" "scikit-learn"))
  ;; matplotlib seems to be broken

  ;; I generated the toolz docset myself

  (set 'python-docsets '("Python 3"
                         "NumPy" "SciPy" "SymPy"
                         "scikit-learn"
                         "toolz"
                         ))

  ;; Add: dash.el pytoolz scikit-learn

  (defun ds/fix-docset-url (x)
    (s-replace " " "_" x))

  (defun ds/installed-docsets ()
    (-map #'ds/fix-docset-url (helm-dash-installed-docsets)))

  (defun install-docsets ()
    (interactive)
    (--> ds/docsets
         (-filter (lambda (x) (not (-contains? (ds/installed-docsets) x))) it)
         (-map #'ds/fix-docset-url it)
         (-map #'helm-dash-install-docset it)))

  (add-hook 'python-mode-hook (lambda () (setq-local helm-dash-docsets python-docsets)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local helm-dash-docsets '("Emacs Lisp"))))

  )


;; Automagically added by customise
;; ============================================================
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cc-other-file-alist
   (quote
    (("\\.cc\\'"
      (".hh" ".h"))
     ("\\.hh\\'"
      (".cc" ".C"))
     ("\\.c\\'"
      (".h"))
     ("\\.h\\'"
      (".cc" ".c" ".C" ".CC" ".cxx" ".cpp"))
     ("\\.C\\'"
      (".H" ".hh" ".h"))
     ("\\.H\\'"
      (".C" ".CC"))
     ("\\.CC\\'"
      (".HH" ".H" ".hh" ".h"))
     ("\\.HH\\'"
      (".CC"))
     ("\\.c\\+\\+\\'"
      (".h++" ".hh" ".h"))
     ("\\.h\\+\\+\\'"
      (".c++"))
     ("\\.cpp\\'"
      (".hpp" ".hh" ".h"))
     ("\\.hpp\\'"
      (".cpp"))
     ("\\.cxx\\'"
      (".hxx" ".hh" ".h"))
     ("\\.hxx\\'"
      (".cxx")))))
 '(column-number-mode t)
 '(ff-ignore-include t)
 '(gud-gdb-command-name "gdb -i=mi --args")
 '(htmlize-output-type (quote font))
 '(indent-tabs-mode nil)
 '(markdown-bold-underscore nil)
 '(org-hide-block-startup t)
 '(org-startup-folded nil)
 '(safe-local-variable-values
   (quote
    ((TeX-master . "../poster")
     (TeX-master . "./main_poster")
     (TeX-master . "../main_poster")
     (TeX-master . t)
     (TeX-master . "main")
     (TeX-master . "./main"))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(yas-wrap-around-region t))


;; When using the "wrong" emacs version colour the modeline red, after
;; everything else to override any customisations.
(when (not ds/emacs-up-to-date?)
  (set-face-attribute 'mode-line nil
                      :background "dark red"))
