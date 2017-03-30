;; Generic emacs settings, other stuff is in individual files in ./lisp/

;; test with compile command: \emacs --debug-init --batch -u $USER

;; TODO:

;; transpose lines easy binding, evil-ify?


;; Fixes to allow rebinding of C-x
;; ============================================================

;; These need to go early on because the rebinds need to be done before the
;; things are loaded.


;; Use C-\ p as prefix for projectile
(defvar projectile-keymap-prefix)
(setq projectile-keymap-prefix (kbd "C-\\ p"))

(global-set-key (kbd "C-\\") ctl-x-map)

(defvar global-edebug-prefix)
(defvar edebug-inhibit-emacs-lisp-mode-bindings)
(setq global-edebug-prefix "\C-\\X")
(setq edebug-inhibit-emacs-lisp-mode-bindings t)
(require 'edebug)

(defvar hi-lock-map)
(setq hi-lock-map (make-sparse-keymap))
(require 'hi-lock)


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


;; Install and load use-package, a nice macro for keeping package config all
;; wrapped up together
(package-install 'use-package)
(require 'ert) ; Need this for now...
(load-library "bind-key")
(load-library "use-package")

;; add some packages for improving package.el
(use-package package-utils :ensure t)
(use-package paradox :ensure t)

;; Get a setq which is checked against defcustom types
(use-package validate :ensure t)

;; Functions/packages used in multiple files
;; ============================================================

(defvar ds/emacs-up-to-date? (or (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
                                 (> emacs-major-version 24))
  "Are we using my prefered emacs version or newer?")

;; some useful libraries
(use-package dash :ensure t)
(use-package s :ensure t)

;; Keep the compiler happy
(require 'dash)
(require 's)

(load-file "~/.emacs.d/lisp/string-manip.el")

(defun ds/switch-to-related (ext &optional current-file-in)
  (let ((current-file (or current-file-in (buffer-file-name))))
    (find-file (concat (file-name-sans-extension current-file) ext))))


;; Config start
;; ============================================================

;; Set up customise

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Pretty colours
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'shepherd t)
;; (load-theme 'adwaita t)


(defun starting-comment-p ()
  "Are we starting to insert a c/java mode comment?"
  (interactive)
  (and (derived-mode-p 'cpp-mode 'java-mode 'c-mode)
       (looking-back "/")))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  ;; Enable in modes where it's safe
  (mapc (lambda (hook) (add-hook hook #'aggressive-indent-mode))
        (list 'c-mode-hook 'c++-mode-hook 'emacs-lisp-mode-hook
              'java-mode-hook 'sh-mode-hook 'ess-mode-hook 'sgml-mode-hook))

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
(validate-setq inhibit-startup-screen t) ;; No startup screen
(scroll-bar-mode -1);; No scroll bar
(validate-setq truncate-partial-width-windows nil) ;; Make line wrapping work for
;; multiple frames
(tool-bar-mode -1) ;; Disable toolbar
(menu-bar-mode -1) ;; Disable menu bar
(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no
(show-paren-mode 1) ;; Highlight matching parenthesis
(setq-default fill-column 80) ;; not 80 because when things are later indented
;; by e.g. diff, git log we lose some columns and
;; it gets messy.
(validate-setq abbrev-mode t) ;; Use abbrev mode always

(require 'etags)
(validate-setq tags-revert-without-query t) ;; Autorevert if the tags table has changed

(validate-setq sentence-end-double-space nil)

;; Shut up and just open symbolic links
(validate-setq vc-follow-symlinks t)

;; Allow some disabled commands
(put 'narrow-to-region 'disabled nil)

;; save point in file
(setq-default save-place t)

;; Auto-newlines after { } etc.
;; (add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-newline 1)))

;; Open urls
(require 'browse-url)
(validate-setq browse-url-browser-function 'browse-url-generic)
(validate-setq browse-url-generic-program "sensible-browser")

;; Wrap lines at nearest word
(diminish 'visual-line-mode)
(global-visual-line-mode 1)


;; Draw a line accross the screen instead of ^L for page breaks
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode t))

(global-linum-mode 1)

;; Show something useful in scratch buffer
(validate-setq initial-scratch-message (emacs-version))

;; Highlight long lines
(use-package whitespace
  ;; builtin
  :diminish whitespace-mode
  :config

  (validate-setq whitespace-line-column 120) ;; limit line length
  (validate-setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode)

  ;; More noticable markings
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
        '(
          (space-mark 32 [9655] [46])
          (newline-mark 10 [8629 10])
          ))


  (defun ds/whitespace-all ()
    (interactive)
    (validate-setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

    (whitespace-mode nil)
    (whitespace-mode t))

  (defun ds/whitespace-sane ()
    (interactive)
    (validate-setq whitespace-style '(spaces tabs newline))
    (whitespace-mode nil)
    (whitespace-mode t))

  )

(use-package hi-lock-mode
  ;; There's also some customisation stuff at the top of init.el to prevent
  ;; it from binding to C-x.

  :config
  (global-hi-lock-mode)

  (defun ds/highlight-spaces-after-tabs ()
    (interactive)
    (highlight-regexp "^		*  *"))

  (add-hook 'prog-mode-hook #'ds/highlight-spaces-after-tabs)
  (add-hook 'sgml-mode-hook #'ds/highlight-spaces-after-tabs))

;; Don't create lockfiles (only safe on single user systems and in ~/ dir
;; technically, but I don't think I'll ever be messing with admin server
;; config files so it's probably fine). Stops emacs spamming up directories
;; with .# files.
(validate-setq create-lockfiles nil)

;; Show keystrokes in progress (eg show C-\ immediately)
(setq echo-keystrokes 0.1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold (* 2 1000 1000 10))

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
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Highlight todo etc.
(defun highlight-todos ()
  (interactive)
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\|\\?\\?ds\\)\\>" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'highlight-todos)
(add-hook 'text-mode-hook (lambda () (when (not (derived-mode-p 'org-mode)) (highlight-todos))))

;; Save point location in files even between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(setq-default tab-width 4)


;; Always try to load the newest version of a file (byte-compiled or not).
(validate-setq load-prefer-newer t)

;; Fonts
;; ============================================================

;; Set the default font
(set-face-attribute 'default '()
                    :family "DejaVu Sans Mono"
                    :height 98)

;; Might be useful at some point
(defun ds/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; isearch
;; ============================================================

;; show number of matches when searching
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config (global-anzu-mode))

;; Treat space-like characters as spaces
(setq search-whitespace-regexp "[-_ 	\n]")

(defun ds/string-symbol-at-point ()
  (symbol-name (or (symbol-at-point)
                   (error "No symbol at point"))))

(defun ds/query-replace-symbol-at-point (replace-str)
  (interactive
   (list (read-string (s-concat "Replace " (ds/string-symbol-at-point) " with: ")
                      nil
                      query-replace-to-history-variable)))
  (let ((symb (ds/string-symbol-at-point)))
    (beginning-of-line) ; Ensure this symbol is included in the replace
    (query-replace symb replace-str)))


;; Camel case word handling
;; ============================================================

;; Treat CamelCase as separate words everywhere, but don't transpose as subwords
(use-package subword
  ;; No ensure: it's built in
  :diminish subword-mode
  :config

  (global-subword-mode 1)
  (define-key subword-mode-map [remap transpose-words] nil)

  ;; Also handle CamelCase in evil mode
  (require 'evil)
  (define-category ?U "Uppercase")
  (define-category ?u "Lowercase")
  (modify-category-entry (cons ?A ?Z) ?U)
  (modify-category-entry (cons ?a ?z) ?u)
  (make-variable-buffer-local 'evil-cjk-word-separating-categories)

  (add-hook 'subword-mode-hook
            (lambda ()
              (if subword-mode
                  (push '(?u . ?U) evil-cjk-word-separating-categories)
                (setq evil-cjk-word-separating-categories
                      (default-value 'evil-cjk-word-separating-categories)))))
  )


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
    (insert (un-camelcase-string camel-word))))


;; Saving
;; ============================================================

;; makes scripts executable automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Make losing focus save buffers
(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (super-save-mode +1))

;; Keep auto saves and backups in one place out of the way
(validate-setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups" t)))
(setq auto-save-list-file-prefix nil)

;; Add a new line at end of file on save if none exists (note: doesn't play
;; nice with scheme).
(setq-default require-final-newline t)


;; Create non-existant directories automatically
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)


;; Use frames instead of emacs "windows"
;; ============================================================
(use-package frames-only-mode
  :ensure t
  :config
  (frames-only-mode 1))

(global-set-key (kbd "s-k") #'make-frame)

;; Copy/Paste interaction with other X11 applications
;; ============================================================

;; Stop selected regions always overwriting the X primary selection (for
;; example when we briefly select the emacs window and it happens to have
;; some text selected from earlier, without this setting the old text would
;; be put into the primary selection). With this setting only explicit
;; selection with the mouse puts things into the primary selection.
(validate-setq select-active-regions 'only)

;; after copy Ctrl+c in X11 apps, you can paste by 'yank' in emacs
(validate-setq select-enable-clipboard t)

;; after mouse selection in X11, you can paste by 'yank' in emacs
(validate-setq select-enable-primary t)

;; Middle click pastes at point not at click position (like in term)
(validate-setq mouse-yank-at-point t)

;; Add things copy pasted from other programs to the kill
(validate-setq save-interprogram-paste-before-kill t)


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
  (validate-setq ac-ignore-case nil)
  (validate-setq ac-use-fuzzy t)
  (validate-setq ac-fuzzy-enable t)
  (ac-flyspell-workaround)

  ;; Show quick help (function info display in tooltip)
  ;; (validate-setq ac-use-quick-help t)
  (validate-setq ac-delay 0.5) ;; show completions quickly

  ;; help is too annoying, press f1 to get it
  ;; ;; (validate-setq ac-show-menu-immediately-on-auto-complete 1)
  ;; (validate-setq ac-quick-help-delay my-ac-delay) ;; show help as soon as it shows
  ;;                                        ;; completions

  ;; let me search even while autocomplete is up!
  (define-key ac-complete-mode-map (kbd "C-s") nil)

  ;; Use f1 to show help in a buffer! Handy :) Don't even need to bind
  ;; anything!
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)

  (validate-setq company-transformers '(company-sort-by-occurrence))
  (global-set-key (kbd "<C-tab>") #'company-complete)

  (defvar ds/global-company-backends)
  (setq ds/global-company-backends
        (list
         'company-capf
         'company-dabbrev-code
         'company-etags
         'company-keywords
         'company-dabbrev
         'company-yasnippet
         'company-files))

  (validate-setq company-backends (list ds/global-company-backends))

  (define-key company-active-map (kbd "C-h") #'company-select-next)
  (define-key company-active-map (kbd "C-i") #'company-select-previous)
  (define-key company-active-map (kbd "<tab>") #'company-complete)
  )

;; Undo tree
;;================================================================
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
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


;; Mode line
;; ============================================================

;; Pretty modeline
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)

  (sml/setup)

  (sml/apply-theme 'dark)

  ;; Shorten some directories to useful stuff
  (add-to-list 'sml/replacer-regexp-list '("^~/oomph-lib/" ":OL:"))
  (add-to-list 'sml/replacer-regexp-list
               '("^~/oomph-lib/user_drivers/micromagnetics" ":OLMM:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/optoomph/" ":OPTOL:"))
  (add-to-list 'sml/replacer-regexp-list
               '("^~/optoomph/user_drivers/micromagnetics" ":OPTOLMM:"))
  )


;; Load my other config files
;; ============================================================

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Load skeletons
(load-file "~/.emacs.d/skeletons.el")

;; Load configs from other files
(load-file "~/.emacs.d/lisp/ds-cpp.el")
(load-file "~/.emacs.d/lisp/ds-latex.el")
(load-file "~/.emacs.d/lisp/ds-scheme.el")
(load-file "~/.emacs.d/lisp/ds-octave.el")
(load-file "~/.emacs.d/lisp/ds-matlab.el")
(load-file "~/.emacs.d/lisp/ds-org.el")
(load-file "~/.emacs.d/lisp/ds-python.el")
(load-file "~/.emacs.d/lisp/ds-unicode-entry.el")
(load-file "~/.emacs.d/lisp/ds-haskell.el")
(load-file "~/.emacs.d/lisp/ds-elisp.el")
(load-file "~/.emacs.d/lisp/ds-java.el")
(load-file "~/.emacs.d/lisp/ds-js.el")
(load-file "~/.emacs.d/lisp/ds-sql.el")
(load-file  "~/.emacs.d/lisp/ds-cmake.el")
(load-file  "~/.emacs.d/lisp/ds-sgml.el")
(load-file  "~/.emacs.d/lisp/ds-css.el")
(load-file  "~/.emacs.d/lisp/ds-sh.el")
(load-file  "~/.emacs.d/lisp/ds-clojure.el")
(load-file  "~/.emacs.d/lisp/ds-typescript.el")

(load-file "~/.emacs.d/lisp/ds-oomph-lib.el")
(load-file  "~/.emacs.d/lisp/ds-biosite.el")

;; Major changes to keybinds
;; Needs to after other file loads so that hooks are in scope
(load-file "~/.emacs.d/lisp/ds-sensible-keys.el")

;; Helm or ido. Goes after keybinds so that we can temporarily override
;; them with helm keys
;; (load-file "~/.emacs.d/lisp/ds-ido.el")
;; (load-file "~/.emacs.d/lisp/ds-helm.el")
(load-file "~/.emacs.d/lisp/ds-ivy.el")



;; This needs to go after SML is loaded so that the mode line colours are
;; correct.
(load-file "~/.emacs.d/lisp/ds-evil.el")

(load-file "~/.emacs.d/lisp/ds-toggle-electricity.el")

;; Save command history between sessions
;; ===============================================================

;; Save into a helpfully named file
(require 'savehist)
(validate-setq savehist-file "~/.emacs.d/savehist")

;; Save other things as well
(validate-setq savehist-additional-variables '(kill-ring
                                               search-ring
                                               regexp-search-ring
                                               compile-command
                                               compilation-directory))

;; Enable save history (must be done after changing any variables).
(savehist-mode 1)

;; Long history for everything
(validate-setq history-length (* 10 1000)) ;; default value for histories without
;; their own variable
(validate-setq kill-ring-max (* 10 1000))
(validate-setq search-ring-max (* 10 1000))
(validate-setq regexp-search-ring-max (* 10 1000))
(validate-setq history-delete-duplicates t)


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

(defvar backup-compile-command nil)

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

   ((derived-mode-p 'js-mode)
    "gulp --silent --reporter=simple --nolint && refresh-browser.sh")

   ;; make is probably a good default for anything else
   (t "make")))

(defun compile-with-default ()
  (interactive)
  ;; Run compile with a specially chosen default command
  (setq backup-compile-command compile-command)
  (setq compile-command (compile-default-command))
  (condition-case nil (call-interactively #'compile)
    ;; If we quit then revert compile command (so that it isn't in the
    ;; history).
    (quit (validate-setq compile-command backup-compile-command))))

(defun toggle-skip-compilation-warnings ()
  (interactive)
  (if (equal compilation-skip-threshold 1)
      (validate-setq compilation-skip-threshold 2)
    (validate-setq compilation-skip-threshold 1)))
(define-key compilation-mode-map (kbd "M-`") #'toggle-skip-compilation-warnings)


;; scroll compilation buffer to first error
(validate-setq compilation-scroll-output 'first-error)

(validate-setq compilation-always-kill t)

;; Autosave all modified buffers before compile
(validate-setq compilation-ask-about-save nil)

;; Handle colours in compile buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "C-<f5>") #'compile-with-default)
(global-set-key (kbd "<M-f5>") #'projectile-compile-project)
(global-set-key (kbd "C-`") #'next-error)
(global-set-key (kbd "C-¬") #'previous-error)


;; My functions etc
;; ============================================================

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

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


(defun generate-org-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch"))
  (org-mode))


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
(validate-setq tramp-default-method "ssh")

;; store backups on my computer
;; (setq tramp-backup-directory-alist  ??ds


;; ;; Auto indent pasted code in programming modes
;; ;; ============================================================
;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;; 	   (and (not current-prefix-arg)
;; 		(member major-mode '(emacs-lisp-mode lisp-mode clojure-mode
;; 						     scheme-mode ruby-mode rspec-mode
;; 						     c-mode c++-mode objc-mode latex-mode
;; 						     plain-tex-mode))
;; 		(let ((mark-even-if-inactive transient-mark-mode))
;; 		  (indent-region (region-beginning) (region-end) nil))))))


;; version control
;; ============================================================

;; git
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")

  :config
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github.

For magit versions > 2.1.0"
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-remote)
                         "url"))
             (cdr (magit-get-remote-branch)))))

  (define-key magit-mode-map (kbd "C-v") #'endless/visit-pull-request-url)

  (validate-setq magit-revert-buffers 'silent)
  (validate-setq magit-save-repository-buffers 'dontask)
  (setq magit-push-always-verify nil) ;; sometimes exists?
  (setq git-commit-major-mode 'markdown-mode)

  (validate-setq git-commit-summary-max-length 100)

  :ensure t)


;; Show changes vs VC in sidebar
(use-package diff-hl
  :ensure t
  :diminish diff-hl-mode
  :init (setq diff-hl-command-prefix (kbd "C-\\ v"))
  :config (global-diff-hl-mode))

(use-package diff-mode
  :config
  (define-key diff-mode-map (kbd "q") #'bury-buffer))

;; Emacs package creation
;; ============================================================

(use-package names-dev
  :ensure names)

(add-to-list 'auto-mode-alist (cons "Cask" #'emacs-lisp-mode))

(use-package hl-sexp
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode))

;; Markdown mode
;; ============================================================

(use-package markdown-mode
  :ensure t
  :config
  ;; run markdown-mode on files ending in .md
  (validate-setq auto-mode-alist
                 (append auto-mode-alist '((".md" . markdown-mode)
                                           (".markdown" . markdown-mode))))
  (defun markdown-mode-keys ()
    (interactive)

    ;; get rid of C-c binds
    (local-set-key (kbd "C-c") nil)

    (local-set-key (kbd "M-n") nil)

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


;; Projectile
;; ============================================================
(use-package projectile
  ;; :ensure t
  :diminish projectile-mode
  :init

  ;; kill all projectile keys
  (validate-setq projectile-mode-map (make-sparse-keymap))

  :config

  ;; Use projectile to open files by default, if available.
  (defun maybe-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (counsel-projectile-find-file)
      (call-interactively #'counsel-find-file)))
  (global-set-key (kbd "C-k") 'maybe-projectile-find-file)

  ;; Use everywhere
  (projectile-global-mode)

  (validate-setq projectile-use-git-grep t)

  (global-set-key (kbd "<f8>") #'projectile-ag)

  (validate-setq projectile-tags-command "ctags-exuberant -eR -f \"%s\" %s")

  (require 'hydra)

  (defun ds/projectile-project-root-nothrow ()
    (condition-case nil
        (projectile-project-root)
      (error nil)))

  (defhydra hydra-projectile (:color teal
                                     :hint nil)
    "
     PROJECTILE: %(or (ds/projectile-project-root-nothrow) \"Not in a project\")

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("s-f" projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("s-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue))

  (define-key projectile-mode-map (kbd "C-\\ p") #'hydra-projectile/body)
  )



;; yasnippet?
;; ============================================================

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config

  ;; Load my snippets
  (validate-setq yas-snippet-dirs (list "~/.emacs.d/snippets"))

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


              (validate-setq yas-fallback-behavior 'return-nil)

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
  (yas-global-mode)

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
  (validate-setq yas-new-snippet-default "\
# -*- mode: snippet; require-final-newline: nil -*-
# key: `(file-name-nondirectory buffer-file-name)`
# --
$0")

  )


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

;; set up tab key
(add-hook 'js-mode-hook 'set-tab)

;; indent by 2
(validate-setq js-indent-level 2)


;; C mode
;; ============================================================

(define-key c-mode-map (kbd "C-c") nil)
(add-hook 'c-mode-hook #'set-tab)


(use-package avy
  :ensure t

  :config
  ;; favour home row keys
  (let ((first-list  '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h)))
    (validate-setq avy-keys
                   (nconc first-list
                          (-difference (loop for i from ?a to ?z collect i) first-list))))

  (validate-setq avy-dispatch-alist nil)

  (global-set-key (kbd "C-p") 'avy-goto-word-1))


(use-package expand-region
  :ensure t
  :bind ("C-a" . er/expand-region))

;; Tagging
;; ============================================================

;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook #'ggtags-mode)
;;   (define-key ggtags-mode-map (kbd "C-c") nil)
;;   (define-key ggtags-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
;;   (validate-setq ggtags-mode-line-project-name nil)
;;   (define-key ggtags-mode-map (kbd "M-<") nil)
;;   (define-key ggtags-mode-map (kbd "M->") nil))


(use-package imenu-anywhere
  :ensure t
  :config
  (global-set-key (kbd "M-,") #'imenu-anywhere)
  )

;; Better help commands
;; ============================================================

;; Show source code for things
(define-key 'help-command (kbd "F") 'find-function)
(define-key 'help-command (kbd "K") 'find-function-on-key)
(define-key 'help-command (kbd "V") 'find-variable)


;; Clean up whitespace
;; ============================================================

;; Automatically clean whitespace in lines that we have touched
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (validate-setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

(use-package aggressive-fill-paragraph
  :load-path "~/.emacs.d/aggressive-fill-paragraph-mode"
  :config
  ;; Enable in modes where it works nicely
  (mapc (lambda (hook) (add-hook hook #'aggressive-fill-paragraph-mode))
        (list 'c-mode-hook
              'c++-mode-hook
              'emacs-lisp-mode-hook
              'java-mode-hook
              'sh-mode-hook
              'python-mode-hook
              'org-mode-hook
              'ess-mode-hook
              'js-mode-hook))

  ;; It's annoying to try to fill multiline sql in c++
  (add-to-list 'afp-fill-comments-only-mode-list 'c++-mode)
  )


;; Packages to help remember keys
;; ============================================================

(use-package discover
  :config (global-discover-mode)
  :ensure t)


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
    ("n" man nil)
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


;; Auto generated for all keys, but not as nicely layed out as
;; hydra/discover-mode.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init

  ;; Popup immediately (magit style)
  (setq which-key-idle-delay 0)

  ;; Don' truncate key names
  (setq which-key-special-keys nil)

  :config
  (which-key-mode)
  )

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
              (validate-setq ess-arg-function-offset t)))

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
              (validate-setq projectile-tags-command
                             "R -e 'rtags(recursive=TRUE,ofile=\"%s\")'")))
  )


(use-package electric-operator
  :load-path "~/.emacs.d/electric-operator"
  :config
  ;; Enable in some modes
  (mapc (lambda (hook) (add-hook hook #'electric-operator-mode))
        '(python-mode-hook
          ess-mode-hook
          c-mode-hook
          c++-mode-hook
          java-mode-hook
          js-mode-hook
          sql-mode-hook
          haskell-mode-hook
          ))

  (validate-setq electric-operator-c-pointer-type-style 'type)

  ;; Disable <, > in C++: support isn't good enough yet
  (electric-operator-add-rules-for-mode 'c++-mode
                                        (cons "<" nil)
                                        (cons ">" nil))
  )

(use-package replace-pairs
  :load-path "~/.emacs.d/replace-pairs")

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(use-package feature-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
  (define-key feature-mode-map (kbd "C-c") nil)
  (add-hook 'feature-mode-hook #'set-tab)
  (add-hook 'feature-mode-hook  (lambda () (define-key orgtbl-mode-map (kbd "C-c") nil)))
  )


;; My lex mode
(use-package lex-mode
  :load-path "~/.emacs.d/lex-mode"
  :config
  (add-to-list 'auto-mode-alist '("\\.lex\\'" . lex-mode))
  (add-to-list 'auto-mode-alist '("\\.flex\\'" . lex-mode))
  :demand)


(use-package helm-dash
  :ensure t
  :config

  (validate-setq helm-dash-min-length 0)

  ;; Add dash.el pytoolz scikit-learn by hand :(

  ;; base docsets
  (setq ds/general-docsets '(
                             "Emacs Lisp"
                             "Markdown"
                             "C"
                             "C++"
                             "PostgreSQL"
                             "CMake"
                             "CSS"
                             ))

  (setq python-docsets '(
                         "Python 3"
                         "NumPy"
                         "SciPy"
                         ;; "SymPy"
                         ;; "scikit-learn"
                         ;; "toolz"
                         ))

  (setq ds/js-docsets '(
                        "MomentJS"
                        "AngularJS"
                        "JavaScript"
                        "CSS"
                        "HTML"
                        "Gulp"
                        "Jasmine"
                        "Lo-Dash"
                        "jQuery"
                        ;; "angular-ui-bootstrap"
                        ))

  (setq ds/shell-docsets '("Bash"))

  (setq ds/docsets
        (-concat ds/general-docsets ds/js-docsets ds/shell-docsets))

  (defun ds/fix-docset-url (x)
    (s-replace " " "_" x))

  (defun ds/installed-docsets ()
    (-map #'ds/fix-docset-url (helm-dash-installed-docsets)))

  (defun ds/install-docsets ()
    (interactive)
    (--> ds/docsets
         (-filter (lambda (x) (not (-contains? (ds/installed-docsets) x))) it)
         (-map #'ds/fix-docset-url it)
         (-each it #'helm-dash-async-install-docset)))

  (defmacro ds/set-docsets-fn (docsets)
    `(lambda () (setq-local helm-dash-docsets ,docsets)))

  (add-hook 'python-mode-hook (ds/set-docsets-fn python-docsets))
  (add-hook 'emacs-lisp-mode-hook (ds/set-docsets-fn '("Emacs Lisp")))
  (add-hook 'c++-mode-hook (ds/set-docsets-fn' ("C++")))
  (add-hook 'js-mode-hook (ds/set-docsets-fn ds/js-docsets))
  (add-hook 'cmake-mode-hook (ds/set-docsets-fn '("CMake")))
  (add-hook 'sql-mode-hook (ds/set-docsets-fn '("PostgreSQL")))
  (add-hook 'sh-mode-hook (ds/set-docsets-fn '("Bash")))
  (add-hook 'css-mode-hook (ds/set-docsets-fn '("CSS")))
  )

(defun ds/insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


(use-package multiple-cursors
  :ensure t
  :config

  (require 'hydra)
  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"

  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))

  )

;; Use rainbow delimeters to highlight mismatched parens.
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :config
  ;; Use everywhere sensible
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; Normal delimeters are normal colour
  (setq rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                      :foreground 'unspecified
                      :inherit 'default)

  ;; Error delimiters get error colour
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(use-package nameless
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))


(use-package beacon
  :disabled
  :ensure t
  :config
  (add-hook 'text-mode-hook 'beacon-mode))

(use-package smooth-scrolling
  :ensure t
  :config

  ;; Keep n lines of context around point (usually...)
  (validate-setq smooth-scroll-margin 3))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config

  ;; Stop flycheck from stomping C-c
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (validate-setq flycheck-keymap-prefix (kbd "C-\\ f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  ;; Setup keybinds
  (define-key flycheck-mode-map (kbd "M-`") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-¬") #'flycheck-previous-error)

  (validate-setq flycheck-display-errors-delay 0.2)
  (validate-setq flycheck-standard-error-navigation nil)

  ;; emacs lisp stuff
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers
                (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))
  (use-package flycheck-cask
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)
    )
  )

(defun ds/disable-mode-if-exists (mode)
  (when (boundp mode)
    (apply mode (list 0))))

(defun ds/disable-electricity ()
  (interactive)
  (ds/disable-mode-if-exists #'aggressive-fill-paragraph-mode)
  (ds/disable-mode-if-exists #'aggressive-indent-mode)
  (ds/disable-mode-if-exists #'electric-indent-mode)
  (ds/disable-mode-if-exists #'electric-operator-mode)
  )


;; Don't indent string blocks
(define-minor-mode suppress-multiline-string-indent-mode
  "Prevent indent commands from indenting multiline strings"

  (defun ds/line-in-string-block? (&rest args)
    "Does this line either end or start as a string?"
    (save-excursion
      (or (progn (beginning-of-line) (nth 3 (syntax-ppss)))
          (progn (end-of-line) (nth 3 (syntax-ppss))))))

  (let ((indent-functions (list #'indent-for-tab-command
                                #'indent-region
                                #'indent-according-to-mode)))
    (if suppress-multiline-string-indent-mode
        (-each indent-functions (lambda (f) (advice-add f :before-until #'ds/line-in-string-block?)))
      (-each indent-functions (lambda (f) (advice-remove f #'ds/line-in-string-block?))))))

(add-hook 'c++-mode-hook #'suppress-multiline-string-indent-mode)
(add-hook 'js-mode-hook #'suppress-multiline-string-indent-mode)
(add-hook 'typescript-mode-hook #'suppress-multiline-string-indent-mode)


(use-package crux
  :ensure t
  )

(use-package ag
  :ensure t
  :config
  ;; Include hidden files
  (add-to-list 'ag-arguments "--hidden")
  (validate-setq ag-group-matches nil)
  )

(use-package scratch
  :ensure t
  )

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :config
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  )

(use-package fill-function-arguments
  :load-path "~/.emacs.d/fill-function-arguments"
  :config
  (add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))

  (add-hook 'sgml-mode-hook (lambda ()
                              (setq-local fill-function-arguments-first-argument-same-line t)
                              (setq-local fill-function-arguments-argument-separator " ")
                              (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
  )

(use-package emerge
  :config
  (add-hook 'emerge-startup-hook #'emerge-fast-mode)
  (add-hook 'emerge-startup-hook #'ds/toggle-electricity--disable)
  )

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit. Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))



;; When using the "wrong" emacs version colour the modeline red, after
;; everything else to override any customisations.
(when (not ds/emacs-up-to-date?)
  (set-face-attribute 'mode-line nil
                      :background "dark red"))

(defun save-and-close-client ()
  (interactive)
  (save-buffer)
  (server-edit))
(global-set-key (kbd "C-\\ #") #'save-and-close-client)

(use-package editorconfig
  :diminish editorconfig-mode
  :init
  (editorconfig-mode))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(use-package ivy
  :ensure t)

(use-package hamburger-menu
  :ensure t
  :init
  (hamburger-menu-mode)
  )


(use-package conf-mode
  :ensure t
  :config
  (add-hook 'conf-mode-hook #'set-tab)

  (add-to-list 'auto-mode-alist (cons "gitconfig$" #'conf-mode))
  (add-to-list 'auto-mode-alist (cons "gitignore$" #'conf-mode))
  (add-to-list 'auto-mode-alist (cons "hgrc$" #'conf-mode)))


(defun fixup-whitespace-prog-mode-dot()
  (when (and (derived-mode-p 'prog-mode)
             (not (nth 8 (syntax-ppss)))
             (looking-at-p "\\s-*\\."))
    (delete-horizontal-space)))
(advice-add #'fixup-whitespace :after #'fixup-whitespace-prog-mode-dot)

(use-package dropbox-conflicts
  :load-path "~/.emacs.d/dropbox-conflicts-el/"
  :config
  (dropbox-conflicts-mode))

(defun ds/reload-dir-locals ()
  (interactive)
  (let ((enable-local-variables :all))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (hack-dir-local-variables-non-file-buffer)))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun ds/add-to-load-path (dir)
  "Interactively add DIR to the load path."
  (interactive "D")
  (add-to-list 'load-path dir))


(use-package hideshowvis
  :load-path "~/.emacs.d/hideshowvis/"
  :config
  (set-face-attribute 'hs-face nil :foreground "saddle brown" :background nil)
  (set-face-attribute 'hs-fringe-face nil :foreground "saddle brown" :background nil)
  (set-face-attribute 'hideshowvis-hidable-face nil :foreground "grey20" :box nil)

  (validate-setq hideshowvis-hidden-marker " ⬵⤁ ")
  (validate-setq hideshowvis-fringe-hidden-marker 'right-arrow)
  (validate-setq hideshowvis-fringe-hideable-marker 'down-arrow)
  (hideshowvis-symbols)

  ;; Turn on for buffer with (hideshowvis-mode), kinda slow...
  )

(use-package terminal-here
  :load-path "~/.emacs.d/terminal-here"
  :config
  (global-set-key (kbd "C-<f7>") #'terminal-here-launch)
  (global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)

  (defun external-shell-in-project-build ()
    "Start urxvt in the current project's build directory"
    (interactive)
    (terminal-here-launch-in-directory (f-join (projectile-project-root) "build")))
  (global-set-key (kbd "<M-f6>") 'external-shell-in-project-build))
