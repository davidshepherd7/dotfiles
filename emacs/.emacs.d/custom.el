(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cl-assert-debug-on-failure t)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("517263bd2a0b32fdbd05be27d0927f1a0f91c96bb8622858d7ba1b8349721dcf" "e667675ba4bc2e12ca8cd37a125746d9b59616a59f381e0f0dc873059c49d86a" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "da41ee46d2f74bb1af6591206b37c07941ee42707299da8058683d3d4917c5b1" default)))
 '(gud-gdb-command-name "gdb -i=mi --args")
 '(htmlize-output-type (quote font))
 '(indent-tabs-mode nil)
 '(markdown-bold-underscore nil)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(minimap-mode t)
 '(org-hide-block-startup t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (graphql-mode dumb-jump terraform-mode lsp-mode blacken forge know-your-http-well kotlin-mode google-translate sql-indent evil-nerd-commenter highlight-indentation scala-mode rjsx-mode evil-goggles slime-company slime racer rainbow-mode rust-mode counsel-dash smartparens tide edit-list bm xelb visual-regexp request circe sqlup-mode sass-mode refine irony modern-cpp-font-lock web-mode php-mode terminal-here package-lint el-mock restclient ivy-hydra counsel-projectile counsel helm-unicode hideshowvis ggtags frames-only-mode less-css-mode vimish-fold undo-tree sed-mode arduino-mode bbdb dockerfile-mode docker swiper lorem-ipsum validate hambuger-menu hambuger-menu-mode ivy subword-mode company company-flx evil-args typescript-mode company-anaconda flycheck flycheck-clojure clojure-mode yasnippet yaml-mode ws-butler which-key wgrep-ag web-beautify use-package super-save smooth-scrolling smex smart-mode-line sequences scratch rtags restart-emacs rainbow-delimiters pos-tip paradox page-break-lines package-utils nlinum nameless multiple-cursors monky mmm-mode matlab-mode magit list-register key-chord julia-mode json-mode js2-mode javadoc-lookup imenu-anywhere ido-ubiquitous hl-sexp highlight-tail highlight-chars helm-projectile helm-ls-hg helm-ls-git helm-descbinds helm-dash haskell-mode go-mode fuzzy flycheck-package flycheck-cask flx-ido feature-mode expand-region evil-surround evil-matchit evil ess discover diff-hl deft debbugs crux control-mode coffee-mode cmake-mode camcorder avy auto-complete auctex anzu ansi aggressive-indent ag ack ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((flycheck-checker quote ds/python-dmypy)
     (flycheck-checker . "ds/python-dmypy")
     (flycheck-python-dmypy-executable . "~/code/monorepo/money-srv/.money-srv-venv/bin/mypy")
     (outline-minor-mode)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (nameless-current-name . electric-operator-)
     (nameless-current-name . electric-operator)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (Syntax . Common-Lisp)
     (projectile-project-compilation-dir . "./build-incremental-link")
     (projectile-project-compilation-dir . "../build")
     (projectile-project-compilation-dir . "./build")
     (projectile-project-compilation-dir . "clang-build/")
     (projectile-project-compilation-dir . "../build/")
     (projectile-project-compilation-cmd . "ninja")
     (projectile-project-relative-compilation-dir . "build/")
     (projectile-project-compilation-dir . "build/")
     (projectile-project-compilation-dir . t)
     (projectile-hg-command . "hg locate -f -0 -I . && hg locate -R common -f -0 -I common/")
     (projectile-hg-command . "hg files -0 -S")
     (TeX-master . "../poster")
     (TeX-master . "./main_poster")
     (TeX-master . "../main_poster")
     (TeX-master . t)
     (TeX-master . "main")
     (TeX-master . "./main"))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(yas-wrap-around-region t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(minimap-active-region-background ((t (:background "gray")))))
