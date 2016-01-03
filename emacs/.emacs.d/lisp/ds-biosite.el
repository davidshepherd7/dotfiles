
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

  ;; Indent by single tab for streaming
  (c-set-offset 'stream-op '+)

  ;; Indent comments by one tab
  (c-set-offset 'c '+)
  )

(add-hook 'c++-mode-hook #'biosite-c-style)


(defun ds/biosite-html-style ()
  (interactive)
  (setq sgml-basic-offset tab-width)
  (setq indent-tabs-mode t)
  )

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


(defun ds/align-comma ()
  (interactive)
  (align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)"))

;; align commas (for PA_SERIALISE macros)
(require 'align)
(add-to-list 'align-rules-list
             '(align-biosite-comma
               (regexp . ",\\(\\s-*\\)")))


               (defun ds/biosite-sql-style ()
                 (interactive)
                 (set 'indent-tabs-mode t))
(add-hook 'sql-mode-hook #'ds/biosite-sql-style)



(defun ds/new-boron-sql ()
  (interactive)
  (let (
        (next-version (-->
                       (f-glob (f-join (projectile-project-root) "boron/shared/db/sql/upgrade_*_to_*.sql"))
                       (-sort (lambda (s1 s2) (not (string-lessp s1 s2))) it)
                       (car it)
                       (f-filename it)
                       (f-base it)
                       (s-split "_" it)
                       (nth 3 it)
                       (string-to-number it)
                       (+ 1 it)
                       ))
        )
    (-->
     (format "upgrade_%03i_to_%03i.sql" (- next-version 1) next-version)
     (f-join (projectile-project-root) "boron/shared/db/sql" it)
     (find-file it))

    (insert (format "UPDATE version SET current = %i;" next-version))
    ))
