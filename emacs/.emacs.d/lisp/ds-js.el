(require 'js)

(set 'js-switch-indent-offset js-indent-level)

(add-to-list 'auto-mode-alist (cons "\\.ts$" #'js-mode))

(defun ds/js-electric-layout-rules ()
  (interactive)
  (add-to-list 'electric-layout-rules (cons ?\; nil))
  (add-to-list 'electric-layout-rules (cons ?{ nil))
  (electric-layout-mode 0))
(add-hook 'js-mode-hook #'ds/js-electric-layout-rules)

(add-hook 'js-mode-hook #'electric-operator-mode)
(add-hook 'js-mode-hook #'aggressive-indent-mode)

(require 'align)
(add-to-list 'align-sq-string-modes 'js-mode)

(require 'grep)
(add-to-list 'grep-find-ignored-files "*.min.js")
(add-to-list 'grep-find-ignored-files "*.min.js.map")

;; Don't really know what these are but they show up in the biosite repo
(add-to-list 'grep-find-ignored-files "*.bmml")

;; Show function as lambda. Append prettify-symbols-mode so that symbol for
;; function is added before the mode is enabled.
(defun ds/pretty-function ()
  (add-to-list 'prettify-symbols-alist '("function" . 955)))
(add-hook 'js-mode-hook #'ds/pretty-function)
(add-hook 'js-mode-hook #'prettify-symbols-mode t)

(font-lock-add-keywords
 'js-mode '(
            ;; Jasmine
            ("\\<\\(expect\\)\\>" 1 font-lock-keyword-face)
            ("\\<\\(it\\)\\>" 1 font-lock-keyword-face)
            ("\\<\\(describe\\)\\>" 1 font-lock-keyword-face)

            ;; Promises
            ("\\.\\<\\(then\\)\\>" 1 font-lock-keyword-face)

            ;; Angular
            ("\\.\\<\\(controller\\)\\>" 1 font-lock-keyword-face)
            ("\\.\\<\\(directive\\)\\>" 1 font-lock-keyword-face)
            ("\\.\\<\\(factory\\)\\>" 1 font-lock-keyword-face)
            ("\\.\\<\\(service\\)\\>" 1 font-lock-keyword-face)

            ))


(defun ds/switch-to-html ()
  (interactive)
  (ds/switch-to-related ".html"))
(define-key js-mode-map (kbd "C-\\ o") #'ds/switch-to-html)

(defun ds/js-switch-to-test ()
  (interactive)
  (if (s-contains? ".spec.js" (buffer-file-name))
      (ds/switch-to-related ".js" (file-name-sans-extension (buffer-file-name)))
    (ds/switch-to-related ".spec.js")))
(define-key js-mode-map (kbd "C-\\ n") #'ds/js-switch-to-test)

;; flycheck config
(require 'flycheck)

(defcustom ds/use-future-eslintrc t "use the eslintrc in boron web_applications?")

(defun ds/maybe-future-eslintrc ()
  (interactive)
  (let ((future-eslint
         (-find #'f-file?
                (list (f-join (projectile-project-root) "boron" "web_applications" "future-eslintrc.json")
                      (f-join (projectile-project-root) "boron" "web_applications" "future-eslintrc-temp.json")))))
    (if (and future-eslint ds/use-future-eslintrc)
        future-eslint
      nil)))

(defun ds/toggle-future-eslintrc ()
  (interactive)
  (flycheck-mode 0)
  (setq-local ds/use-future-eslintrc (not ds/use-future-eslintrc))
  (setq-local flycheck-eslintrc (ds/maybe-future-eslintrc))
  (flycheck-mode 1))

(defun ds/js-flycheck-setup ()
  (interactive)
  (setq-local flycheck-checker 'javascript-eslint)
  (setq-local flycheck-eslintrc (ds/maybe-future-eslintrc))
  (add-to-list 'flycheck-disabled-checkers 'javascript-jscs)
  (flycheck-mode))
(add-hook 'js-mode-hook #'ds/js-flycheck-setup)



(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . js-mode))



;; Parse typescript compiler output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(typescript "\\(.*\\)(\\([0-9]*\\),\\([0-9]*\\)): " 1 2))
(add-to-list 'compilation-error-regexp-alist 'typescript)


(use-package json-mode
  :ensure t
  :config

  (defun setup-json-indent ()
    (interactive)
    (setq-local indent-tabs-mode nil)
    (setq-local js-indent-level 2))
  (add-hook 'json-mode-hook #'setup-json-indent)

  (defun ds/set-json-mode (fname)
    (add-to-list 'auto-mode-alist (cons fname 'json-mode)))
  (-map #'ds/set-json-mode (list ".bowerrc" ".jscsrc"))
  )


;; Support es6 string literals
(modify-syntax-entry ?` "\"" js-mode-syntax-table)
