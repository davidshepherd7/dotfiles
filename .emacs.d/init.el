;; Generic emacs settings, other stuff is in individual files in ./my-files/


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
(setq completion-cycle-threshold 5) ;; Cycle through completion options if there
                                    ;; are only a few.
(setq tags-revert-without-query 1) ;; Autorevert if the tags table has changed

;; Add a new line at end of file on save if none exists (note: doesn't play
;; nice with scheme).
(setq-default require-final-newline t)

;; Autosave all modified buffers before compile
(setq compilation-ask-about-save nil)

;; Had to remove this because oomph-lib doesn't stick to no trailing whitespace...,
;; Remove trailing whitespace before save
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Display minibuffer completions in the minbuffer (to avoid needing extra
;; frames to see them).
(icomplete-mode 1)


;; Auto complete
;;================================================================
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(ac-config-default)
(setq ac-ignore-case nil)
(setq ac-use-fuzzy t)
(setq ac-fuzzy-enable t)
(ac-flyspell-workaround)

;; Show quick help (function info display in tooltip)
(setq ac-use-quick-help t)

;; let me search even while autocomplete is up!
(define-key ac-complete-mode-map (kbd "C-s") nil)

;; Use alt tab for fuzzy complete (need to unbind other uses first).
(define-key ac-complete-mode-map (kbd "M-TAB") nil)
(define-key ac-mode-map (kbd "M-TAB") nil)
(define-key abbrev-map (kbd "M-TAB") nil)
(global-set-key (kbd "M-TAB") 'ac-fuzzy-complete)

;; Add clang source
(require 'auto-complete-clang)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (add-to-list 'ac-sources 'ac-source-clang)))

;; ;; yasnippet
;; ;; ============================================================
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet/snippets")

;; ;; (add-hook 'snippet-mode-hook
;; ;;           (lambda () (local-set-key (kbd "<f5>") 'yas/tryout-snippet)))


;; Undo tree
;;================================================================
(load-file "~/.emacs.d/undo-tree/undo-tree-0.1.6.elc")
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-map '())

;; Unbind commands that might be hurting my hands
;; ============================================================
(global-set-key (kbd "C-x C-s") '())
(global-set-key (kbd "C-x s") '())
(global-set-key (kbd "C-x k") '())
(global-set-key (kbd "C-x b") '())


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
(load-file "~/.emacs.d/my-files/maxima.el")
(load-file "~/.emacs.d/my-files/buffer-cycling.el")

;; Load major changes to keybinds
(load-file "~/.emacs.d/my-files/sensible-keys.el")

;; General keybinds
;; ===============================================================
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


;; File open keybinds
;; ===============================================================
(global-set-key (kbd "C-<f12>")
                '(lambda () (interactive) (find-file "~/.bashrc")))
(global-set-key (kbd "C-<f11>")
                '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-<f10>")
                '(lambda () (interactive) (find-file "~/.emacs.d/abbrev_defs")))
(global-set-key (kbd "C-<f9>")
                '(lambda () (interactive) (find-file "~/.emacs.d/skeletons.el")))
(global-set-key (kbd "C-<f8>")
                '(lambda () (interactive) (find-file "~/.xmonad/xmonad.hs")))
(global-set-key (kbd "C-<f7>")
                '(lambda () (interactive)
                   (find-file "~/Dropbox/linux_setup/Ubuntu_install_script.sh")))

(global-set-key (kbd "C-<f6>") 'ibuffer)
(global-set-key (kbd "C-<f5>")
                '(lambda () (interactive) (find-file "~/Dropbox/org/uni.org")))

(global-set-key (kbd "C-<f1>") 'oomph-open-files)


;; Save command history between sessions
;; ===============================================================
;; Save other things as well
(setq savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      compile-command))

;; Save into a helpfully named file
(setq savehist-file "~/.emacs.d/savehist")

;; Enable (must be after changing any variables).
(savehist-mode 1)


;; Compile mode settings
;; ===============================================================
(add-hook 'compilation-mode-hook
          '(lambda()
             (local-set-key (kbd "<f5>") 'recompile)
             (local-set-key (kbd "C-`") 'next-error)
             (local-set-key (kbd "C-¬") 'previous-error)))

;; Edit with emacs (integrate with chrome)
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

(set 'edge-background-colour "grey2")

;; Set all the areas around the edge to be slightly lighter
(set-face-background 'modeline edge-background-colour)
(set-face-background 'modeline-inactive edge-background-colour)
(set-face-background 'fringe edge-background-colour)
(set-face-background 'linum edge-background-colour)
(set-face-background 'menu edge-background-colour)

;; To get rid of the box around inactive modeline I used custom to set it
;; to the same colour... hacky :(

;; Nice dim line number font colour
(set-face-foreground 'linum "grey20")

;; Automagically added by customise
;; ============================================================
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(htmlize-output-type (quote font))
 '(indent-tabs-mode nil)
 '(markdown-bold-underscore nil)
 '(minibuffer-complete-cycle t nil (minibuffer-complete-cycle)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-inactive ((t (:inherit mode-line :background "grey2" :foreground "grey80" :box (:line-width -1 :color "grey2") :weight light)))))


;; Auto indent pasted code in programming modes
;; ============================================================
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;; Package management
;; ============================================================

;; Add melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
(when (>= emacs-major-version 24)
  (require 'page-break-lines)
  (global-page-break-lines-mode 't))

;; Ctags
;; ============================================================

(setq ctags-command "ctags -e --recurse")
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
