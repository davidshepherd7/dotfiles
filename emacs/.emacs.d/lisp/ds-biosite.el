
(defun ds/disable-tabs-advice (&rest args)
  "Use spaces not tabs for advised function"
  (let ((indent-tabs-mode nil))
    (apply args)))
(advice-add #'align :around #'ds/disable-tabs-advice)
(advice-add #'align-regexp :around #'ds/disable-tabs-advice)
(advice-add #'align-region :around #'ds/disable-tabs-advice)
;; Add any other alignment functions here


(defun biosite-c-style ()
  (interactive)

  (set 'indent-tabs-mode t)

  ;; Ensure that one offset is a tab
  (set 'c-basic-offset tab-width)

  (c-set-offset 'substatement-open '0)

  ;; Indent extra function argument line by 1 tab
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-intro '+)

  ;; Don't indent trailing `)` in functions
  (c-set-offset 'arglist-close '0)

  ;; Don't indent braces for enums
  (c-set-offset 'brace-list-open 0)
  )

(add-hook 'c++-mode-hook #'biosite-c-style)


(defun ds/biosite-html-style ()
  (interactive)
  (setq sgml-basic-offset tab-width)
  (setq indent-tabs-mode t)
  (setq tab-stop-list '(4 8 12 16)))

(add-hook 'html-mode-hook #'ds/biosite-html-style)


(defun ds/biosite-js-style ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq js-indent-level tab-width))
(add-hook 'js-mode-hook #'ds/biosite-js-style)


(defun ds/biosite-cmake-style ()
  (interactive)
  (setq indent-tabs-mode t))
(add-hook 'cmake-mode-hook #'ds/biosite-cmake-style)

(require 'grep)
(add-to-list 'grep-find-ignored-directories "boron/web_applications/app")
(add-to-list 'grep-find-ignored-directories "boron/web_applications/dev-app")
(add-to-list 'grep-find-ignored-directories "build/")


(defun ds/biosite-test-to-main ()
  (--> (buffer-file-name)
       (file-relative-name it (projectile-project-root))
       (replace-regexp-in-string "/tests/" "/" it)
       (replace-regexp-in-string "\.cpp" ".h" it)))

;; align commas (for PA_SERIALISE macros)
(require 'align)
(add-to-list 'align-rules-list
             '(align-biosite-comma
               (regexp . ",\\(\\s-*\\)[^\\s-*]")
               (group . 1)
               (modes . '(c-mode c++-mode))
               (repeat . nil)))
