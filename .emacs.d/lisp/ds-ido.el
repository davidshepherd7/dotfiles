;; Improving Auto complete in minibuffer
;; ============================================================


;; Use ido
(use-package ido
  :ensure t

  :config
  (progn
    (ido-mode t)

    ;; (for all buffer/file name entry)
    (ido-everywhere)

    ;; and for some other places
    (use-package ido-ubiquitous
      :ensure t
      :config (ido-ubiquitous-mode))

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
    ))

;; Even better fuzzy search for ido
(use-package flx-ido
  :ensure t
  :config (progn (flx-ido-mode 1)
		 (setq ido-use-faces nil)))

;; smex: ido based completion for commands
(use-package smex
  :ensure t
  :config (progn ;; Change the main keybinding
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
	      (define-key ido-completion-map (kbd "M-.") 'smex-find-function))))


;; keybinds
;; ============================================================

(let ((my-new-map (make-sparse-keymap)))

  ;; (define-key my-new-map (kbd "RET") 'ido-complete)
  (set 'ido-common-completion-map my-new-map)

  ;; clear all the extra keymaps
  (set 'ido-completion-map (make-sparse-keymap))
  (set 'ido-buffer-completion-map (make-sparse-keymap))
  (set 'ido-file-completion-map (make-sparse-keymap))
  (set 'ido-file-dir-completion-map (make-sparse-keymap))
  (set 'ido-file-dir-completion-map (make-sparse-keymap)))


(global-set-key (kbd "C-S-k") 'find-file)
(global-set-key (kbd "M-k") 'switch-to-buffer)
