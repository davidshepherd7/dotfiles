
(require 'use-package)

(defun ds//hafnium-path-to-import (path)
  (interactive)
  (--> path
       (s-trim it)
       (f-join (projectile-project-root) it)
       (file-relative-name it (projectile-project-root))
       (s-split-up-to "src/" it 2)
       (nth 1 it)
       (s-concat "./" it)
       (s-chop-suffix ".ts" it)
       (s-chop-suffix ".js" it)))

(defun ds//select-ts-import ()
  (let* ((headers (--> (projectile-current-project-files)
                       (-filter (lambda (filepath) (or (s-ends-with-p ".ts" filepath)
                                                  (s-ends-with-p ".js" filepath))) it)))
         (default-input (when (symbol-at-point) (symbol-name (symbol-at-point))))
         (file (completing-read "import: " headers nil nil default-input)))
    (ds//hafnium-path-to-import file)))

(defun ds/ts-import ()
  (interactive)
  (goto-char (point-max))
  (re-search-backward "^import" nil t)
  (end-of-line)
  (insert "\n")
  (yas-expand-snippet (yas-lookup-snippet "import")))


(use-package typescript-mode
  :config

  (require 'flycheck)
  (require 'ds-js)
  (require 'typescript-mode)

  (add-hook 'typescript-mode-hook #'electric-operator-mode)

  (add-to-list 'auto-mode-alist (cons "\\.tsx" #'typescript-mode))

  ;; tide handles formatting on save
  (add-hook 'typescript-mode-hook (lambda () (aggressive-indent-mode 0)))

  (require 'align)
  (add-to-list 'align-sq-string-modes 'typescript-mode)

  (add-hook 'typescript-mode-hook #'ds/pretty-function)
  (add-hook 'typescript-mode-hook #'prettify-symbols-mode t)

  (js-extra-keywords 'typescript-mode)

  (define-key typescript-mode-map (kbd "C-\\ o") #'ds/switch-to-html)
  (define-key typescript-mode-map (kbd "C-\\ n") #'ds/js-switch-to-test)
  (define-key typescript-mode-map (kbd "C-,") #'ds/ts-import)

  (defun boron-typings-index ()
    (f-join (projectile-project-root) "boron" "web_applications" "typings" "index.d.ts"))

  (flycheck-define-checker typescript-tsc
    "A TypeScript syntax checker using tsc command."
    :command ("tsc" "--out" "/dev/null" (eval (boron-typings-index)) source)
    :error-patterns
    ((error line-start (file-name) "(" line "," column "): error " (message) line-end))
    :next-checker (typescript-tslint)
    :modes (typescript-mode))
  (add-to-list 'flycheck-checkers 'typescript-tsc)

  ;; Parse typescript compiler output
  (require 'compile)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(typescript "\\(\\S-*\\)\\s-*(\\([0-9]*\\),\\([0-9]*\\)): " 1 2))
  (add-to-list 'compilation-error-regexp-alist 'typescript)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "at [^( ]+ (\\(.*?\\):\\([0-9]*\\):\\([0-9]*\\))" 1 2))
  (add-to-list 'compilation-error-regexp-alist 'node)

  ;; Remove some auto indent stuff (aggressive-indent-mode is enough)
  (validate-setq typescript-auto-indent-flag nil)
  (-each (list (kbd ",") (kbd "(") (kbd ")") (kbd ":") (kbd ";") (kbd "{"))
    (lambda (key) (define-key typescript-mode-map key nil)))

  (defun ds/setup-typescript-fill-function-arguments ()
    (setq-local fill-function-arguments-trailing-separator t))
  (add-hook 'typescript-mode-hook #'ds/setup-typescript-fill-function-arguments)


  (defun ds/find-flycheck-checker-node-modules (executable)
    (let* ((node-modules (locate-dominating-file (or (buffer-file-name) default-directory) "node_modules/"))
           (exec-path (if node-modules
                          (cons (f-join node-modules "node_modules" ".bin") exec-path)
                        exec-path)))
      (flycheck-default-executable-find executable)))
  (defun ds/use-find-flycheck-checker-node-modules ()
    (setq-local flycheck-executable-find #'ds/find-flycheck-checker-node-modules))
  (add-hook 'typescript-mode-hook #'ds/use-find-flycheck-checker-node-modules)
  )


(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)

    (flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode)

    ;; Rely on tide for formatting
    (aggressive-indent-mode 0)

    (local-set-key (kbd "C-,") #'tide-fix)

    (validate-setq tide-format-before-save nil)

    )

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook #'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

