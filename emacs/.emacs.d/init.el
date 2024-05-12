
;; Generic emacs settings, other stuff is in individual files in ./lisp/

;; test with compile command: \emacs --debug-init --batch -u $USER

;; TODO:

;; transpose lines easy binding, evil-ify?


;; Fixes to allow rebinding of C-x
;; ============================================================

;; These need to go early on because the rebinds need to be done before the
;; things are loaded.


;; ;; Use C-\ p as prefix for projectile
;; (defvar projectile-keymap-prefix)
;; (setq projectile-keymap-prefix (kbd "C-\\ p"))

(global-set-key (kbd "C-\\") ctl-x-map)

(defvar global-edebug-prefix)
(defvar edebug-inhibit-emacs-lisp-mode-bindings)
(setq global-edebug-prefix "\C-\\X")
(setq edebug-inhibit-emacs-lisp-mode-bindings t)
(setq lsp-keymap-prefix "M-#")
(require 'edebug)

(defvar hi-lock-map)
(setq hi-lock-map (make-sparse-keymap))
(require 'hi-lock)


;; Set up "straight.el" package manager etc.
;; ============================================================


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and load use-package, a nice macro for keeping package config all
;; wrapped up together
(straight-use-package 'use-package)
(require 'straight)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-vc-git-force-protocol t)
(require 'use-package)

(use-package diminish)

;; Get a setq which is checked against defcustom types
(use-package validate)

(use-package dash)

;; Have to load org first so that we don't accidentally load the non-straight
;; version of it
(load-file "~/.emacs.d/lisp/ds-org.el")

;; Functions/packages used in multiple files
;; ============================================================

(defvar ds/emacs-up-to-date? (or (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
                                 (> emacs-major-version 24))
  "Are we using my prefered emacs version or newer?")

;; some useful libraries
(use-package dash)
(use-package s)
(use-package f)

;; Keep the compiler happy
(require 'dash)
(require 's)
(require 'f)

(load-file "~/.emacs.d/lisp/string-manip.el")


;; Config start
;; ============================================================

;; Set up customise

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq garbage-collection-messages t)

;; Pretty colours
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'shepherd t)

(validate-setq debugger-stack-frame-as-list t)

(defun starting-comment-p ()
  "Are we starting to insert a c/java mode comment?"
  (interactive)
  (and (derived-mode-p 'cpp-mode 'java-mode 'c-mode)
       (looking-back "/")))

(use-package aggressive-indent

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
(setq-default save-place-mode t)

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
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode t))

(if (>= emacs-major-version 26)
    ;; validate-setq complains, for some reason
    (setq-default display-line-numbers t)
  (global-linum-mode 1))

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

;; (use-package hi-lock-mode
;;   :disable
;;   ;; There's also some customisation stuff at the top of init.el to prevent
;;   ;; it from binding to C-x.
;;   :config
;;   (global-hi-lock-mode)

;;   (defun ds/highlight-spaces-after-tabs ()
;;     (interactive)
;;     (highlight-regexp "^		*  *"))

;;   (add-hook 'prog-mode-hook #'ds/highlight-spaces-after-tabs)
;;   (add-hook 'sgml-mode-hook #'ds/highlight-spaces-after-tabs))

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
(validate-setq revert-without-query '(".*"))

;; ;; Automatically add newlines after certain characters (e.g. '{')
;; (electric-layout-mode)

;; Make the rules for electric layout local to buffers (so that different
;; modes don't interfere)
(make-variable-buffer-local 'electric-layout-rules)

;; Automatically add closing character where appropriate
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Highlight todo etc.
(defun highlight-todos ()
  (interactive)
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|TODO9\\|BUG\\|NOTE\\|HACK\\|\\?\\?ds\\)\\>" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'highlight-todos)
(add-hook 'text-mode-hook (lambda () (when (not (derived-mode-p 'org-mode)) (highlight-todos))))
(add-hook 'conf-mode-hook 'highlight-todos)

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
  :diminish anzu-mode
  :config (global-anzu-mode))

;; Treat space-like characters as spaces
;; (setq search-whitespace-regexp "[-_ 	\n]")

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
  (define-key subword-mode-map [remap transpose-words] nil))


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
  :diminish super-save-mode
  :config
  (super-save-mode +1)

  (validate-setq super-save-max-buffer-size 80000)
  )

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
  :config
  (frames-only-mode 1))


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

(defun ds/yank-from-diff ()
  "As yank, but remove diff symbols from lines"
  (interactive)
  (insert (replace-regexp-in-string "^[+-]" "" (current-kill 0))))


;; Auto complete
;;================================================================
(use-package fuzzy :disabled)
(use-package pos-tip :disabled)
(use-package auto-complete
  :disabled
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
  :diminish company-mode
  :config
  (global-company-mode)

  (validate-setq company-transformers '(company-sort-by-occurrence))
  (global-set-key (kbd "<C-tab>") #'company-complete)
  (global-set-key (kbd "M-]") #'company-files)

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

  ;; Case sensitive simple completions
  (require 'company-dabbrev)
  (validate-setq company-dabbrev-downcase nil)
  )

(global-set-key (kbd "C-M-i") #'hippie-expand)

;; Undo tree
;;================================================================
(use-package undo-tree
  :diminish undo-tree-mode
  :demand
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :config

  ;; Global undo tree mode doesn't work because I've rebound the usual undo keys
  (add-hook 'text-mode-hook (lambda () (undo-tree-mode 1)))
  (add-hook 'prog-mode-hook (lambda () (undo-tree-mode 1)))
  ;; conf-mode is neither text nor prog!
  (add-hook 'conf-mode-hook (lambda () (undo-tree-mode 1)))
  (add-hook 'minibuffer-setup-hook (lambda () (undo-tree-mode 1)))

  ;; clean out the undo-tree keymap entry in the keymap list
  (let ((item (assoc 'undo-tree-mode minor-mode-map-alist)))
    (setf (cdr item) (make-sparse-keymap)))

  ;; For consistency make the keymap itself empty as well
  (setf undo-tree-map (make-sparse-keymap))

  (validate-setq undo-tree-enable-undo-in-region nil)
  )


;; Mode line
;; ============================================================

;; Pretty modeline
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)

  (sml/setup)

  (sml/apply-theme 'dark)
  )


(use-package hydra
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

;; Projectile
;; ============================================================
(use-package projectile
  :diminish projectile-mode
  :init

  ;; kill all projectile keys
  (setq projectile-mode-map (make-sparse-keymap))

  :config

  ;; Use projectile to open files by default, if available.
  (defun maybe-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (counsel-projectile-find-file)
      (call-interactively #'counsel-find-file)))
  (global-set-key (kbd "C-k") 'maybe-projectile-find-file)

  ;; Use everywhere
  (projectile-mode)

  (validate-setq projectile-use-git-grep t)

  (global-set-key (kbd "<f8>") #'projectile-ag)
  (global-set-key (kbd "C-\\ o") #'projectile-toggle-between-implementation-and-test)

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
(validate-setq history-length 500) ;; default value for histories without
;; their own variable
(validate-setq kill-ring-max 500)
(validate-setq search-ring-max 500)
(validate-setq regexp-search-ring-max 500)
(validate-setq history-delete-duplicates nil)


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

(defun ds/compile-default-command ()
  (interactive)
  (cond
   ;; Files with hashbang should be run
   ((and (buffer-has-hashbang) (buffer-file-name))
    (concat "./" (file-name-nondirectory (buffer-file-name))))

   ;; emacs lisp should be tested by running a new emacs instance in batch mode
   ((derived-mode-p 'emacs-lisp-mode) "\\emacs --debug-init --batch -u $USER")

   ((derived-mode-p 'tex-mode)
    (concat "latexmk -C && latexmk -pdf" " " (file-name-nondirectory (buffer-file-name))))

   ((derived-mode-p 'typescript-mode 'javascript-mode) "npm run build")

   ;; Some config files
   ((equal (buffer-name) "sxhkdrc") "killall sxhkd -SIGUSR1")
   ((equal (buffer-name) ".conkyrc") "killall conky -SIGUSR1")

   ;; make is probably a good default for anything else
   (t "make")))

(defun compile-with-default ()
  (interactive)
  ;; Run compile with a specially chosen default command
  (setq backup-compile-command compile-command)
  (setq compile-command (ds/compile-default-command))
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

(defun copy-project-file-path ()
  "Add path relative to project root to the kill ring."
  (interactive)
  (if (not (stringp buffer-file-name))
      (error "Not visiting a file.")
    (let ((project-relative-path (f-relative buffer-file-name (projectile-project-root))))
      (kill-new project-relative-path)
      ;; Give some visual feedback:
      (message "String \"%s\" saved to kill ring." project-relative-path)
      project-relative-path)))

(defun copy-project-file-line ()
  "Add something like boron/foo/bar.cpp:123 to the kill ring."
  (interactive)
  (if (not (stringp buffer-file-name))
      (error "Not visiting a file.")
    (let* ((project-relative-path (f-relative buffer-file-name (projectile-project-root)))
           (copy-string (s-concat project-relative-path ":" (number-to-string (line-number-at-pos)))))
      (kill-new copy-string)
      ;; Give some visual feedback:
      (message "String \"%s\" saved to kill ring." copy-string)
      copy-string)))


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
(require 'tramp)
(validate-setq tramp-default-method "ssh")

;; store backups on my computer
;; (setq tramp-backup-directory-alist  ??ds


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

  ;; (validate-setq magit-revert-buffers 'silent)
  (validate-setq magit-save-repository-buffers 'dontask)
  ;; (setq magit-push-always-verify nil) ;; sometimes exists?

  (validate-setq git-commit-summary-max-length 120)

  ;; Move prev/next message commands because the overlap normal keys
  (define-key git-commit-mode-map (kbd "M-n") nil)
  (define-key git-commit-mode-map (kbd "M-p") nil)
  (define-key git-commit-mode-map (kbd "C-c p") #'git-commit-prev-message)
  (define-key git-commit-mode-map (kbd "C-c n") #'git-commit-next-message)

  (global-set-key (kbd "<M-f7>") #'magit-log-buffer-file)


  ;; Remove some magit keys that interfere
  (define-key magit-mode-map (kbd "M-w") nil))

;; (use-package forge
;;   :straight nil
;;   :after magit)

(use-package git-link
  ;; Use my fork for now
  :straight
  (git-link :type git :host github :repo "davidshepherd7/git-link" :branch "permalink-support")
  :config
  (validate-setq git-link-open-in-browser nil)
  (validate-setq git-link-default-branch "dev")
  (validate-setq git-link-use-commit nil)
  (validate-setq git-link-branch-permalinks t)
  )


;; Show changes vs VC in sidebar
(use-package diff-hl
  :diminish diff-hl-mode
  :init (setq diff-hl-command-prefix (kbd "C-\\ v"))
  :config (global-diff-hl-mode))

(use-package diff-mode
  :config
  (define-key diff-mode-map (kbd "q") #'bury-buffer))

;; Emacs package creation
;; ============================================================

;; (use-package names-dev
;;   ;; This is loaded by names
;;   :straight nil
;;   :ensure names)

(add-to-list 'auto-mode-alist (cons "Cask" #'emacs-lisp-mode))

(use-package hl-sexp
  :config
  (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode))

;; Markdown mode
;; ============================================================

(use-package markdown-mode
  :config
  ;; run markdown-mode on files ending in .md
  (validate-setq auto-mode-alist
                 (append auto-mode-alist '((".md" . markdown-mode)
                                           (".markdown" . markdown-mode))))
  (defun markdown-mode-keys ()
    (interactive)

    (local-set-key (kbd "M-n") nil)
    (local-set-key (kbd "C-i") nil)

    ;; Compile = preview
    (local-set-key [remap compile] 'markdown-preview)
    (local-set-key [remap my-recompile] 'markdown-preview)

    )
  (add-hook 'markdown-mode-hook 'markdown-mode-keys)

  (define-key markdown-mode-map (kbd "C-c") nil)

  (defun ds/locally-keep-ws-before-point ()
    (validate-setq ws-butler-keep-whitespace-before-point t))
  (add-hook 'markdown-mode-hook #'ds/locally-keep-ws-before-point)

  (defun ds/disable-electric-indent-for-markdown ()
    (electric-indent-mode 0))
  (add-hook 'markdown-mode-hook #'ds/disable-electric-indent-for-markdown)
  )

;; Bind goto last change
;; ============================================================

(use-package goto-last-change
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
  :config
  (global-set-key (kbd "C-\\ r v") 'list-register)
  (global-set-key (kbd "C-\\ r s") 'copy-to-register)
  (global-set-key (kbd "C-\\ r i") 'insert-register))


;; yasnippet?
;; ============================================================

(use-package yasnippet
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
    (yas--make-directory-maybe (car (yas--guess-snippet-directories))))

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
  :config
  (setq deft-directory "~/Dropbox/notes")
  (setq deft-extension "md")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (validate-setq deft-auto-save-interval 0.0) ; disable

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

(use-package avy
  :config
  ;; favour home row keys
  (let ((first-list  '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h)))
    (validate-setq avy-keys
                   (nconc first-list
                          (-difference (cl-loop for i from ?a to ?z collect i) first-list))))

  (validate-setq avy-dispatch-alist nil)

  (global-set-key (kbd "C-p") 'avy-goto-word-1))


(use-package expand-region
  :bind ("C-a" . er/expand-region))

;; Tagging
;; ============================================================

;; (use-package ggtags
;;   :config
;;   (add-hook 'prog-mode-hook #'ggtags-mode)
;;   (define-key ggtags-mode-map (kbd "C-c") nil)
;;   (define-key ggtags-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
;;   (validate-setq ggtags-mode-line-project-name nil)
;;   (define-key ggtags-mode-map (kbd "M-<") nil)
;;   (define-key ggtags-mode-map (kbd "M->") nil))


;; (use-package imenu-anywhere
;;   :config
;;   (global-set-key (kbd "M-,") #'imenu-anywhere)
;;   )

(global-set-key (kbd "M-[") #'imenushow-paren-context-when-offscreen)

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
  :diminish ws-butler-mode
  :config
  (validate-setq ws-butler-keep-whitespace-before-point nil)
  (make-variable-buffer-local 'ws-butler-keep-whitespace-before-point)
  (add-hook 'org-mode-hook #'ds/locally-keep-ws-before-point)

  ;; Don't use globally: I mostly use autoformtters for code now anyway
  (add-hook 'text-mode-hook #'ws-butler-mode)
  )

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
              ;; 'org-mode-hook
              'ess-mode-hook
              'js-mode-hook))

  ;; It's annoying to try to fill multiline sql in c++
  (add-to-list 'afp-fill-comments-only-mode-list 'c++-mode)
  )

(defun ds/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;; Packages to help remember keys
;; ============================================================

(use-package discover
  :config (global-discover-mode))


;; Auto generated for all keys, but not as nicely layed out as
;; hydra/discover-mode.
(use-package which-key
  :diminish which-key-mode
  :init

  ;; Popup immediately (magit style)
  (setq which-key-idle-delay 0)

  ;; Don' truncate key names
  (setq which-key-special-keys nil)

  :config
  (which-key-mode)
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
                                        (cons ">" nil)
                                        (cons ":" nil)
                                        (cons "::" nil))

  ;; Doxygen comments
  (electric-operator-add-rules-for-mode 'c++-mode (cons "///" "/// "))

  (electric-operator-add-rules-for-mode 'ess-mode (cons "^" "^"))
  )

(use-package replace-pairs
  :load-path "~/.emacs.d/replace-pairs")

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (validate-setq yaml-indent-offset 2)
  (add-hook 'yaml-mode-hook #'set-tab)

  (add-hook 'yaml-mode-hook (lambda () (validate-setq indent-tabs-mode nil)))
  (add-hook 'yaml-mode-hook (lambda () (validate-setq yaml-indent-offset 2)))
  (add-hook 'yaml-mode-hook #'flycheck-mode)
  )

(use-package feature-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
  (define-key feature-mode-map (kbd "C-c") nil)
  (add-hook 'feature-mode-hook #'set-tab)
  (add-hook 'feature-mode-hook  (lambda () (define-key orgtbl-mode-map (kbd "C-c") nil)))
  )


;; (use-package helm-dash
;;   :config

;;   (validate-setq helm-dash-min-length 0)

;;   ;; Add dash.el pytoolz scikit-learn by hand :(

;;   ;; base docsets
;;   (setq ds/general-docsets '(
;;                              "Emacs Lisp"
;;                              "Markdown"
;;                              "C"
;;                              "C++"
;;                              "PostgreSQL"
;;                              "CMake"
;;                              "CSS"
;;                              ))

;;   (setq ds/python-docsets '(
;;                             "Python 3"
;;                             "SQLAlchemy"
;;                             ;; "NumPy"
;;                             ;; "SciPy"
;;                             ;; "SymPy"
;;                             ;; "scikit-learn"
;;                             ;; "toolz"
;;                             ))

;;   (setq ds/js-docsets '(
;;                         "MomentJS"
;;                         "AngularJS"
;;                         "JavaScript"
;;                         "CSS"
;;                         "HTML"
;;                         "Gulp"
;;                         "Jasmine"
;;                         "Lo-Dash"
;;                         "jQuery"
;;                         ;; "angular-ui-bootstrap"
;;                         ))

;;   (setq ds/html-docsets '(
;;                           "CSS"
;;                           "HTML"
;;                           "Bootstrap 3"
;;                           ))

;;   (setq ds/shell-docsets '("Bash"))

;;   (setq ds/ansible-docsets '("Ansible"))

;;   (defun ds/docsets ()
;;     (-concat ds/general-docsets ds/js-docsets ds/shell-docsets ds/html-docsets ds/ansible-docsets ds/python-docsets))

;;   (defun ds/fix-docset-url (x)
;;     (s-replace " " "_" x))

;;   (defun ds/installed-docsets ()
;;     (-map #'ds/fix-docset-url (helm-dash-installed-docsets)))

;;   (defun ds/install-docsets ()
;;     (interactive)
;;     (--> (ds/docsets)
;;          (-filter (lambda (x) (not (-contains? (ds/installed-docsets) x))) it)
;;          (-map #'ds/fix-docset-url it)
;;          (-each it #'helm-dash-async-install-docset)))

;;   (defmacro ds/set-docsets-fn (docsets)
;;     `(lambda () (setq-local helm-dash-docsets ,docsets)))

;;   (add-hook 'python-mode-hook (ds/set-docsets-fn ds/python-docsets))
;;   (add-hook 'emacs-lisp-mode-hook (ds/set-docsets-fn '("Emacs Lisp")))
;;   (add-hook 'c++-mode-hook (ds/set-docsets-fn' ("C++")))
;;   (add-hook 'js-mode-hook (ds/set-docsets-fn ds/js-docsets))
;;   (add-hook 'typescript-mode-hook (ds/set-docsets-fn ds/js-docsets))
;;   (add-hook 'cmake-mode-hook (ds/set-docsets-fn '("CMake")))
;;   (add-hook 'sql-mode-hook (ds/set-docsets-fn '("PostgreSQL")))
;;   (add-hook 'sh-mode-hook (ds/set-docsets-fn '("Bash")))
;;   (add-hook 'css-mode-hook (ds/set-docsets-fn '("CSS")))
;;   (add-hook 'html-mode-hook (ds/set-docsets-fn ds/html-docsets))
;;   (add-hook 'yaml-mode-hook (ds/set-docsets-fn ds/ansible-docsets))
;;   )

(defun ds/insert-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun ds/insert-current-date-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ")))

;; Use rainbow delimeters to highlight mismatched parens.
(use-package rainbow-delimiters
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
  :config
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  (define-key nameless-mode-map (kbd "_") #'nameless-insert-name-or-self-insert)

  ;; Don't affect indentation, otherwise other people's Emacs indent the code
  ;; differently to mine.
  (validate-setq nameless-affect-indentation-and-filling nil)
  )


(use-package beacon
  :disabled
  :config
  (add-hook 'text-mode-hook 'beacon-mode))

(use-package smooth-scrolling
  :config

  ;; Keep n lines of context around point (usually...)
  (validate-setq smooth-scroll-margin 3))

(use-package flycheck
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
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)
    )

  ;; This message popup is annoying
  (validate-setq flycheck-checker-error-threshold 2000)
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
(defun ds/line-in-string-block? (&rest args)
  "Does this line either end or start as a string?"
  (save-excursion
    (or (progn (beginning-of-line) (nth 3 (syntax-ppss)))
        (progn (end-of-line) (nth 3 (syntax-ppss))))))
(define-minor-mode suppress-multiline-string-indent-mode
  "Prevent indent commands from indenting multiline strings"
  :init-value nil
  (let ((indent-functions (list #'indent-for-tab-command
                                #'indent-region
                                #'indent-according-to-mode)))
    (if suppress-multiline-string-indent-mode
        (-each indent-functions (lambda (f) (advice-add f :before-until #'ds/line-in-string-block?)))
      (-each indent-functions (lambda (f) (advice-remove f #'ds/line-in-string-block?))))))

(add-hook 'c++-mode-hook #'suppress-multiline-string-indent-mode)
(add-hook 'js-mode-hook #'suppress-multiline-string-indent-mode)
(add-hook 'typescript-mode-hook #'suppress-multiline-string-indent-mode)


(use-package crux)

(use-package ag
  :config
  ;; Include hidden files
  (add-to-list 'ag-arguments "--hidden")
  (validate-setq ag-group-matches nil)
  )

(use-package highlight-symbol
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

  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq-local fill-function-arguments-first-argument-same-line t)
                                    (setq-local fill-function-arguments-second-argument-same-line t)
                                    (setq-local fill-function-arguments-last-argument-same-line t)
                                    (setq-local fill-function-arguments-argument-separator " ")))
  )

(use-package emerge
  :config
  (add-hook 'emerge-startup-hook #'emerge-fast-mode)
  (add-hook 'emerge-startup-hook #'ds/toggle-electricity--disable)
  )

;; (use-package ediff
;;   :config

;;   (defun ds/kill-ediff-buffers ()
;;     (kill-buffer ediff-buffer-A)
;;     (kill-buffer ediff-buffer-B)
;;     (kill-buffer ediff-buffer-C)
;;     (kill-buffer "*ediff-errors*")
;;     (kill-buffer "*ediff-merge*")
;;     (kill-buffer "*ediff-diff*")
;;     (kill-buffer "*Ediff Control Panel*"))

;;   (add-hook 'ediff-quit-hook #'ds/kill-ediff-buffers)

;;   ;; Automatically save back merges when done, always.
;;   (setq-default ediff-autostore-merges t)

;;   (validate-setq ediff-keep-variants nil)
;;   (validate-setq ediff-make-buffers-readonly-at-startup t)
;;   (add-hook 'ediff-prepare-buffer-hook #'ds/toggle-electricity--disable)
;;   )

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

(use-package swiper
  :init
  (global-set-key (kbd "C-S-f") #'swiper))

(use-package conf-mode
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

;; (use-package dropbox-conflicts
;;   :load-path "~/.emacs.d/dropbox-conflicts-el/"
;;   :config
;;   (dropbox-conflicts-mode))

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


;; (use-package hideshowvis
;;   :load-path "~/.emacs.d/hideshowvis/"
;;   :config
;;   (set-face-attribute 'hs-face nil :foreground "saddle brown" :background nil)
;;   (set-face-attribute 'hs-fringe-face nil :foreground "saddle brown" :background nil)
;;   (set-face-attribute 'hideshowvis-hidable-face nil :foreground "grey20" :box nil)

;;   (validate-setq hideshowvis-hidden-marker " ⬵ ")
;;   (validate-setq hideshowvis-fringe-hidden-marker 'right-arrow)
;;   (validate-setq hideshowvis-fringe-hideable-marker 'down-arrow)
;;   (hideshowvis-symbols)

;;   ;; Turn on for buffer with (hideshowvis-mode), kinda slow...
;;   )

(use-package terminal-here
  :load-path "~/.emacs.d/terminal-here"
  :bind (("C-<f7>" . terminal-here-launch)
         ("C-<f6>" . terminal-here-project-launch))
  :config
  (validate-setq terminal-here-linux-terminal-command 'urxvt))

(use-package robot-mode
  :load-path "~/.emacs.d/vc-packages/robot-mode/")


;; (use-package php-mode
;;   :config
;;   (define-key php-mode-map (kbd "M-e") nil)
;;   (define-key php-mode-map (kbd "C-e") nil)

;;   (add-hook 'php-mode-hook (lambda () (flycheck-mode)))
;;   (add-hook 'php-mode-hook (lambda () (flycheck-select-checker 'php)))

;;   )


;; (use-package smalltalk-mode
;;   :load-path "~/.emacs.d/vc-packages/smalltalk-mode/smalltalk-mode.el"
;;   :config
;;   (add-to-list 'auto-mode-alist (cons "\.st$" #'smalltalk-mode)))


;; https://github.com/k-talo/volatile-highlights.el

;; (use-package volatile-highlights
;;   :config
;;   (volatile-highlights-mode)
;;   )

(use-package highlight-indentation
  :config
  (add-hook 'python-mode-hook #'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook #'highlight-indentation-mode)

  (set-face-background 'highlight-indentation-face "gray15")
  (set-face-background 'highlight-indentation-current-column-face "gray28")
  )

(use-package midnight
  :config
  ;; Clean up every hour
  (validate-setq midnight-period (* 60 60))

  ;; Clean these buffers after an hour
  (validate-setq clean-buffer-list-delay-special (* 60 60))
  (add-to-list 'clean-buffer-list-kill-regexps "\\*Help\\*")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*ag search ")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*.*[Ee]diff")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*tramp/sudo ")

  ;; Clean other buffers every day
  (validate-setq clean-buffer-list-delay-general 1)
  )


(use-package visual-regexp
  :config

  (defalias 'visual-regexp-replace #'vr/replace)
  (defalias 'visual-regexp-query-replace #'vr/query-replace)
  (defalias 'visual-regexp-mc-mark #'vr/mc-mark)

  (global-set-key (kbd "M-'") #'visual-regexp-query-replace)
  )

(use-package anki-mode
  :load-path  "~/.emacs.d/vc-packages/anki-mode"
  )

;; (use-package nand2tetris
;;   :load-path "~/.emacs.d/vc-cackages/nand2tetris.el"
;;   :config
;;   (add-to-list 'auto-mode-alist (cons "\.hdl$" #'nand2tetris-mode))
;;   (validate-setq nand2tetris-core-base-dir "~/Dropbox/education/nand2tetris"))

(defun ds/insert-uuid ()
  (interactive)
  (insert (s-trim (shell-command-to-string "uuidgen"))))

(defun ds/insert-nil-uuid ()
  (interactive)
  (insert "00000000-0000-0000-0000-000000000000"))


(defun ds/table-to-array (begin end)
  (interactive "r")
  "Convert a whitespace separated table to [] array"
  (let ((result (--> (buffer-substring-no-properties begin end)
                     (s-split "\n" it nil)
                     (--map (replace-regexp-in-string "\\s-+" ", " it) it)
                     (--map (s-concat "[" it "]") it)
                     (s-join ",\n" it)
                     (s-concat "[" it "]"))))
    (delete-region begin end)
    (insert result)))


;; tla operators
(apply #'electric-operator-add-rules-for-mode 'tla-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'tla-mode
                                      (cons "/\\" " /\\ ")
                                      (cons "\\/" " \\/ ")
                                      (cons "|->" " |-> ")
                                      (cons "\\in" " \\in ")
                                      (cons "<<" " << ")
                                      (cons ">>" " >> ")
                                      )




(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


(defun ds/tick-current-test ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (kill-word 2)
    (delete-char 1)
    (insert ":white_check_mark:")))

(defun ds/tick-first-test ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^:arrow_right:")
    (ds/tick-current-test)))

(use-package google-translate
  :config
  (validate-setq google-translate-default-source-language "French")
  (validate-setq google-translate-default-target-language "English")

  ;; This... kinda works. It seems to give worse translations than the UI and I
  ;; haven't found a nice way to get the end of the string.
  (defun ds/translate-string ()
    (interactive)
    (let* ((start-of-string (+ (nth 8 (syntax-ppss)) 1))
           (end-of-string (point))
           (json (google-translate-request
                  "fr"
                  "en"
                  (buffer-substring-no-properties start-of-string end-of-string)))
           (translated (google-translate-json-translation json)))
      (kill-region start-of-string end-of-string)
      (insert translated)))

  )
(use-package dumb-jump
  :config
  (validate-setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package make-mode
  :config
  (validate-setq makefile-mode-map (make-sparse-keymap)))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package graphql-mode
  :config
  (validate-setq graphql-indent-level 4))

(defun ds/insert-ansi-code ()
  (interactive)
  (let* ((colours '(("Default" . "0")
                    ("Black" . "0;30")
                    ("Red" . "0;31")
                    ("Green" . "0;32")
                    ("Yellow" . "0;33")
                    ("Blue" . "0;34")
                    ("Magenta" . "0;35")
                    ("Cyan" . "0;36")
                    ("White" . "0;37")))
         (colour-name (completing-read "Colour: " (map-keys colours)))
         (colour-code (map-elt colours colour-name nil #'equal)))
    (insert (concat "\\033[" colour-code "m"))))

;; for testsin
(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "python %s" (file-name-nondirectory
                                              buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(use-package wgrep
  :config
  ;; For some reason I needed to load this manually
  (add-to-list 'load-path "~/.emacs.d/straight/repos/Emacs-wgrep")
  (require 'wgrep-ag)
  )
(use-package terraform-mode)
(use-package kotlin-mode)
(use-package dockerfile-mode)
(use-package swift-mode)

(use-package back-button
  :config
  (back-button-mode 1)
  (define-key back-button-mode-map (kbd "<M-left>") #'back-button-local-backward)
  (define-key back-button-mode-map (kbd "<M-right>") #'back-button-local-forward)

  ;; Push more marks to have more places to go back to
  (defun ds/quiet-push-mark (&optional _) (push-mark nil t))
  (add-hook 'isearch-mode-hook #'ds/quiet-push-mark)
  (advice-add #'flycheck-jump-to-error :before #'ds/quiet-push-mark)
  (advice-add #'next-error :before #'ds/quiet-push-mark)

  )

;; A better, more general replacement for blacken-mode etc.
(use-package apheleia
  ;; melpa seems to be broken right now for this package?
  ;; :straight (apheleia :host github :repo "radian-software/apheleia")
  :config
  (apheleia-global-mode +1)
  )

(use-package scala-mode)

(defun ds/switch-to-non-org-buffer ()
  "For use from the window manager to open something that isn't my todo list."
  (interactive)
  (switch-to-buffer nil)
  ;; If it's an org buffer then pop the buffer stack again
  (when (s-ends-with? ".org" buffer-file-name)    
    (switch-to-buffer nil)))


;; Minibuffer config

(defun ds/insert-buffer-file-path ()
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))
(define-key minibuffer-mode-map (kbd "C-.") #'ds/insert-buffer-file-path)


;; Only for testing frames-only-mode config
;;
;; (use-package po-mode
;;   :config
;;   (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*foo*")
;;   (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list '(regexp . "\\*.*\\.po\\*"))
;;   )

(defun ds/find-file-with-line-number-around (orig-fun filename &rest args)
  "Advice for file-file functions such that when given filenames
like foo.py:123 emacs opens foo.py at line 123."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      (apply orig-fun filename args)
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))
        ))))

(advice-add #'find-file :around #'ds/find-file-with-line-number-around)
(advice-add #'find-file-other-frame :around #'ds/find-file-with-line-number-around)
(advice-add #'find-file-other-window :around #'ds/find-file-with-line-number-around)


;; Feature checks
;; ============================================================


(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

;; Load my other config files
;; ============================================================

(add-to-list 'load-path "~/.emacs.d/lisp")

;; This needs to go after SML is loaded so that the mode line colours are
;; correct, but before language configs so that they can have access to keymaps.
(load-file "~/.emacs.d/lisp/ds-evil.el")



;; Load configs from other files
(load-file "~/.emacs.d/lisp/ds-cpp.el")
(load-file "~/.emacs.d/lisp/ds-latex.el")
(load-file "~/.emacs.d/lisp/ds-scheme.el")
(load-file "~/.emacs.d/lisp/ds-octave.el")
(load-file "~/.emacs.d/lisp/ds-matlab.el")
(load-file "~/.emacs.d/lisp/ds-python.el")
(load-file "~/.emacs.d/lisp/ds-cl.el")
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
(load-file  "~/.emacs.d/lisp/ds-rust.el")
(load-file  "~/.emacs.d/lisp/ds-ess.el")

(load-file "~/.emacs.d/lisp/ds-oomph-lib.el")
(load-file  "~/.emacs.d/lisp/ds-wave.el")

;; Major changes to keybinds
;; Needs to after other file loads so that hooks are in scope
(load-file "~/.emacs.d/lisp/ds-sensible-keys.el")

;; Helm or ido. Goes after keybinds so that we can temporarily override
;; them with helm keys
;; (load-file "~/.emacs.d/lisp/ds-ido.el")
(load-file "~/.emacs.d/lisp/ds-helm.el")
(load-file "~/.emacs.d/lisp/ds-ivy.el")

;; TODO: make this a package?
(load-file "~/.emacs.d/lisp/s-interactive.el")

(load-file "~/.emacs.d/lisp/ds-lsp.el")
(load-file "~/.emacs.d/lisp/ds-treesitter.el")
