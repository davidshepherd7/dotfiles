
(require 'sgml-mode)
(require 'js)
(require 'dash)
(require 's)
(require 'f)
(require 'projectile)
(require 'sass-mode)

(require 'align)
(defun ds/disable-tabs-advice (&rest args)
  "Use spaces not tabs for advised function"
  (let ((indent-tabs-mode nil))
    (apply args)))
(advice-add #'align :around #'ds/disable-tabs-advice)
(advice-add #'align-regexp :around #'ds/disable-tabs-advice)
(advice-add #'align-region :around #'ds/disable-tabs-advice)
;; Add any other alignment functions here


;; (defun ds/boron-cpp-ctags ()
;;   (interactive)
;;   (shell-command "find -name '*.cpp' -o -name '*.h' | xargs ctags-exuberant -e -a --force-language=c++")
;;   )


;; Highlight spaces for indentation
(defun ds/highlight-space-indents (activate)
  (let ((regex "^[ 	]*? [ 	]*"))
    (if activate
        (highlight-regexp regex)
      (unhighlight-regexp regex))))

(defun ds/fix-space-indents ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[	]* [ 	]*" nil t)
      (replace-match "")
      (indent-for-tab-command))))


(defun ds/comment-offset (dummy)
  "No offset for closing comment lines"
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-+\\*/") 0 '+)))

(defun ds/single-indent-when-nested (langelem)
  (if (> (length (c-guess-basic-syntax)) 1) 0 '+))

(defun ds/single-unindent-when-nested (langelem)
  (if (> (length (c-guess-basic-syntax)) 1) 0 '-))

(defun ds/template-unindent-trailing-closing-paren (langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*>")
        0
      '+)))

(defun ds/biosite-c-style ()
  (interactive)

  (set 'indent-tabs-mode t)

  ;; Ensure that one offset is a tab
  (set 'c-basic-offset tab-width)

  (c-set-offset 'substatement-open '0)

  ;; Indent extra function argument line by 1 tab
  (c-set-offset 'arglist-cont-nonempty #'ds/single-indent-when-nested)
  (c-set-offset 'arglist-intro '+)

  ;; Don't indent trailing `)` in functions
  (c-set-offset 'arglist-close '0)

  ;; Don't indent braces for enums
  (c-set-offset 'brace-list-open 0)

  ;; Indent by single tab for streaming
  (c-set-offset 'stream-op '+)

  ;; Indent comments by one tab normally, or none on closing comment lines.
  (c-set-offset 'c #'ds/comment-offset)

  (c-set-offset 'template-args-cont #'ds/template-unindent-trailing-closing-paren)
  )

(add-hook 'c++-mode-hook #'ds/biosite-c-style)


(defun ds/biosite-sgml-style ()
  (interactive)
  (setq sgml-basic-offset tab-width)
  (setq indent-tabs-mode t)
  )

(add-hook 'sgml-mode-hook #'ds/biosite-sgml-style)


(defun ds/biosite-js-style ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq js-indent-level tab-width))
(add-hook 'js-mode-hook #'ds/biosite-js-style)
(add-hook 'typescript-mode-hook #'ds/biosite-js-style)


(require 'cmake-mode)
(defun ds/biosite-cmake-style ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq cmake-tab-width 4))
(add-hook 'cmake-mode-hook #'ds/biosite-cmake-style)

(require 'grep)
(add-to-list 'grep-find-ignored-directories "boron/web_applications/app")
(add-to-list 'grep-find-ignored-directories "boron/web_applications/dev-app")
(add-to-list 'grep-find-ignored-directories "build/")


;; Path manipulation
;; ============================================================

(defun ds/biosite-test-to-main (path)
  (--> path
       (replace-regexp-in-string "/tests/" "/" it)
       (s-chop-suffix ".cpp" it)
       (s-concat it ".h")))

(defun ds/biosite--in-tests-dir (path)
  (-first #'f-exists?
          (list (ds/biosite--to-test-helper (file-name-directory path) "tests" (file-name-nondirectory path))
                (ds/biosite--to-test-helper (file-name-directory path) ".." "tests" (file-name-nondirectory path))
                (ds/biosite--to-test-helper (file-name-directory path) ".." ".." "tests" (file-name-nondirectory path))
                )
          ))

(defun ds/biosite--to-test-helper (&rest args)
  (--> (apply #'f-join args)
       (s-chop-suffix ".h" it)
       (s-chop-suffix ".cpp" it)
       (s-concat it ".cpp")))

(defun ds/biosite-main-to-test (path)
  (ds/biosite--in-tests-dir path))

(defun ds/biosite-file-test-main ()
  (interactive)
  (cond
   ((s-contains? "test" (buffer-file-name))
    (find-file (ds/biosite-test-to-main (buffer-file-name))))
   (t
    (pp (ds/biosite-main-to-test (buffer-file-name)))
    (find-file (ds/biosite-main-to-test (buffer-file-name))))))

(define-key c++-mode-map (kbd "C-\\ n") #'ds/biosite-file-test-main)

(defun ds/biosite-test-include-path ()
  (interactive)
  (--> (buffer-file-name)
       (ds/biosite-test-to-main it)
       (file-relative-name it (projectile-project-root))))


(defun ds/biosite-path-to-class-name (path)
  (interactive)
  (--> path
       (file-name-nondirectory it)
       (file-name-sans-extension it)))

;; C++ headers
;; ============================================================

(defun ds/biosite-path-to-include (path)
  (interactive)
  (--> path
       (s-trim it)
       (f-join (projectile-project-root) it)
       (file-relative-name it (projectile-project-root))
       (s-chop-prefix "boron/" it)
       (s-chop-prefix "common/" it)
       (s-concat "#include \"" it "\"")))

(defun ds/biosite-insert-as-include (filepath)
  (save-excursion
    (goto-char (point-min))
    (forward-line 4) ; Should get us past any initial stuff
    (end-of-line)
    (insert "\n")
    (let ((include-line (ds/biosite-path-to-include filepath)))
      (insert include-line)
      (message "Added include: %s" include-line))))

(defun ds/biosite-paste-as-include ()
  (interactive)
  (ds/biosite-insert-as-include (current-kill 0 t)))

(defun ds/biosite-include (suppress-sort)
  (interactive "P")
  (let* ((headers (--> (projectile-current-project-files)
                       (-filter (lambda (filepath) (or (s-ends-with-p ".h" filepath)
                                                  (s-ends-with-p ".hpp" filepath))) it)))
         (default-input (when (symbol-at-point) (symbol-name (symbol-at-point))))
         (file (completing-read "header: " headers nil nil default-input)))
    (ds/biosite-insert-as-include file)
    (when (not suppress-sort)
      (ds/sort-headers))))


;; js template paths
;; ============================================================
(defun ds/biosite-relative-web-path (path)
  (let ((web-root (f-join (projectile-project-root) "boron" "web_applications")))
    (--> path
         (s-trim it)
         (file-relative-name it web-root))))

(defun ds/chop-web-suffixes (path)
  (--> path
       (s-trim it)
       (s-chop-suffix ".spec.js" it)
       (s-chop-suffix ".js" it)
       (s-chop-suffix ".html" it)))

(defun ds/biosite-path-to-html-template-path (path)
  (interactive)
  (--> path
       (ds/biosite-relative-web-path it)
       (ds/chop-web-suffixes it)
       (s-concat "./" it ".html")))

(defun ds/biosite-guess-template-path ()
  (interactive)
  (insert (ds/biosite-path-to-html-template-path (buffer-file-name))))

(defun ds/biosite-path-to-js-path (path)
  (interactive)
  (--> path
       (ds/biosite-relative-web-path it)
       (ds/chop-web-suffixes it)
       (s-concat "./" it ".js")))

(defun ds/s-space-words (str)
  "Hopefully this will be in s.el soon"
  (s-join " " (s-split-words str)))

(defun ds/biosite-path-to-module-label (path)
  (--> path
       (ds/chop-web-suffixes it)
       (file-name-nondirectory it)
       (ds/s-space-words it)))


;; C++ namespaces
;; ============================================================

(defvar ds/biosite-dir-to-ns '())

(set 'ds/biosite-dir-to-ns
     '(
       ;; report_server
       ("report_server" "reports")
       ("json_reporters" "json")
       ("reporters" "")
       ("helpers" "helper")

       ;; event server
       ("event_server" "")

       ;; ("test" "")
       ("audit" "")
       ("tests" "")

       ("db_types" "db")
       ("shared" "")
       ))

(defun ds/biosite-dir-to-namespace (dir)
  (let ((found (assoc dir ds/biosite-dir-to-ns)))
    (if found (cadr found) dir)))

(defun ds/biosite-path-to-namespaces (path)
  (--> path
       (file-name-directory it)
       (s-replace "boron/audit/xenon" "xenon" it) ;; hack for now
       (s-split "/" it t)
       (-map #'ds/biosite-dir-to-namespace it)
       (-filter (lambda (ns) (not (equal ns ""))) it)))

(defun ds/biosite-get-namespaces  (path)
  "Get a space-separated list of namespaces for file at path"
  (interactive)
  (ds/biosite-path-to-namespaces
   (file-relative-name path (projectile-project-root))))

(defun ds/biosite-open-namespaces ()
  (interactive)
  (apply #'s-concat
         (-map
          (lambda (ns) (s-concat "namespace " ns "\n{\n"))
          (ds/biosite-get-namespaces (buffer-file-name)))))

(defun ds/biosite-close-namespaces ()
  (interactive)
  (apply #'s-concat
         (-map
          (lambda (ns) (s-concat "\n} // namespace " ns "\n"))
          (reverse (ds/biosite-get-namespaces (buffer-file-name))))))

(defun ds/biosite-cpp-namespaces ()
  (interactive)
  (--> (ds/biosite-get-namespaces (buffer-file-name))
       (-map (lambda (ns) (s-concat "using namespace " ns ";")) it)
       (s-join "\n" it)))


(defun ds/biosite-make-qualified-class-name (path)
  "Get the fully-qualified class name for path"
  (--> path
       (ds/biosite-get-namespaces it)
       (s-join "::" it)
       (s-concat it "::" (ds/biosite-path-to-class-name path))))

(defun ds/biosite-insert-current-namespace ()
  "Insert the biosite namespace for the current file"
  (interactive)
  (insert (ds/biosite-make-qualified-class-name (file-name-directory (buffer-file-name)))))




(defun ds/align-comma ()
  (interactive)
  (align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)"))

;; align commas (for PA_SERIALISE macros)
(require 'align)
(add-to-list 'align-rules-list
             '(align-biosite-comma
               (regexp . ",\\(\\s-*\\)")
               (repeat . t)))

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

    (insert (format "UPDATE version SET current = %i;" next-version))))


(defun ds/open-with-qt-designer ()
  (interactive)
  (start-process "qtdesigner.sh" nil "qtdesigner.sh" (buffer-file-name)))


;; sgml indentation
;; ============================================================

(defvar ds/sgml-fancy-align nil)

(defvar ds/hanging-close-tags t)

(defun ds/biosite-sgml-calculate-indent (&optional lcon)
  "Calculate the column to which this line should be indented.
LCON is the lexical context, if any."
  (unless lcon (setq lcon (sgml-lexical-context)))

  ;; Indent comment-start markers inside <!-- just like comment-end markers.
  (if (and (eq (car lcon) 'tag)
           (looking-at "--")
           (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
      (setq lcon (cons 'comment (+ (cdr lcon) 2))))

  (pcase (car lcon)

    (`string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (cdr lcon))
                 (zerop (forward-line -1))
                 (looking-at "[ \t]*$")))

     (if (> (point) (cdr lcon))
         ;; Previous line is inside the string.
         (current-indentation)

       (goto-char (cdr lcon))
       (if ds/sgml-fancy-align
           (1+ (current-column))
         (+ (current-indentation) sgml-basic-offset))))

    (`comment
     (let ((mark (looking-at "--")))
       ;; Go back to previous non-empty line.
       (while (and (> (point) (cdr lcon))
                   (zerop (forward-line -1))
                   (or (looking-at "[ \t]*$")
                       (if mark (not (looking-at "[ \t]*--"))))))
       (if (> (point) (cdr lcon))
           ;; Previous line is inside the comment.
           (skip-chars-forward " \t")
         (goto-char (cdr lcon))
         ;; Skip `<!' to get to the `--' with which we want to align.
         (search-forward "--")
         (goto-char (match-beginning 0)))
       (when (and (not mark) (looking-at "--"))
         (forward-char 2) (skip-chars-forward " \t"))
       (current-column)))

    ;; We don't know how to indent it.  Let's be honest about it.
    (`cdata nil)
    ;; We don't know how to indent it.  Let's be honest about it.
    (`pi nil)

    (`tag
     (if (and (looking-at ">") ds/hanging-close-tags)
         ;; Use opening tag's indentation
         (save-excursion (goto-char (cdr lcon)) (current-indentation))
       ;; else
       (goto-char (1+ (cdr lcon)))
       (skip-chars-forward "^ \t\n")	;Skip tag name.
       (skip-chars-forward " \t")
       (cond
        ((and ds/sgml-fancy-align (not (eolp)))
         (current-column))
        (ds/sgml-fancy-align
         (goto-char (1+ (cdr lcon)))
         (+ (current-column) sgml-basic-offset))
        (t
         (goto-char (cdr lcon))
         (+ (current-indentation) sgml-basic-offset)))))

    (`text
     (while (looking-at "</")
       (forward-sexp 1)
       (skip-chars-forward " \t"))
     (let* ((here (point))
            (unclosed (and ;; (not sgml-xml-mode)
                       (looking-at sgml-tag-name-re)
                       (assoc-string (match-string 1)
                                     sgml-unclosed-tags 'ignore-case)
                       (match-string 1)))
            (context
             ;; If possible, align on the previous non-empty text line.
             ;; Otherwise, do a more serious parsing to find the
             ;; tag(s) relative to which we should be indenting.
             (if (and (not unclosed) (skip-chars-backward " \t")
                      (< (skip-chars-backward " \t\n") 0)
                      (back-to-indentation)
                      (> (point) (cdr lcon)))
                 nil
               (goto-char here)
               (nreverse (sgml-get-context (if unclosed nil 'empty)))))
            (there (point)))
       ;; Ignore previous unclosed start-tag in context.
       (while (and context unclosed
                   (eq t (compare-strings
                          (sgml-tag-name (car context)) nil nil
                          unclosed nil nil t)))
         (setq context (cdr context)))
       ;; Indent to reflect nesting.
       (cond
        ;; If we were not in a text context after all, let's try again.
        ((and context (> (sgml-tag-end (car context)) here))
         (goto-char here)
         (sgml-calculate-indent
          (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
                    (sgml-tag-type (car context)) 'tag)
                (sgml-tag-start (car context)))))
        ;; Align on the first element after the nearest open-tag, if any.
        ((and context
              (goto-char (sgml-tag-end (car context)))
              (skip-chars-forward " \t\n")
              (< (point) here) (sgml-at-indentation-p))
         (current-column))
        (t
         (goto-char there)
         (+ (current-column)
            (* sgml-basic-offset (length context)))))))

    (_
     (error "Unrecognized context %s" (car lcon)))))

;; lol, monkey patching
(fset #'sgml-calculate-indent #'ds/biosite-sgml-calculate-indent)


;; Set up js indentation
;; ============================================================

(require 'js)

(defvar ds/js-fancy-align nil
  "Set non-nil to enable alignment of function args etc.")

(defcustom ds/js-indent-chain t
  "Should we auto indent .then and similar when they are on a new line?")

(defvar ds/js-non-indented-line-regexs '("^\\s-*(function\\s-*()\\s-*{")
  "Lines matching this regex set the current indentation level to zero.")

(defun non-indented-line (line)
  (-any (lambda (reg) (string-match-p reg line))
        ds/js-non-indented-line-regexs))

(defun ds/js-chain-p ()
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\.")))

(defun ds/js--continued-expression-p ()
  "Modified version of js--continued-expression-p to return false on .then etc"
  (and
   (js--continued-expression-p)
   (or (not (ds/js-chain-p))
       ds/js-indent-chain)))

(defun ds/biosite-js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p  (ds/js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (cond
              ((non-indented-line (buffer-substring (point-at-bol) (point-at-eol)))
               0)
              ((or (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)") (not ds/js-fancy-align))
               (progn ; nothing following the opening paren/bracket
                 (skip-syntax-backward " ")
                 (when (eq (char-before) ?\)) (backward-list))
                 (back-to-indentation)
                 (js--maybe-goto-declaration-keyword-end parse-status)
                 (let* ((in-switch-p (unless same-indent-p
                                       (looking-at "\\_<switch\\_>")))
                        (same-indent-p (or same-indent-p
                                           (and switch-keyword-p
                                                in-switch-p)))
                        (indent
                         (cond (same-indent-p
                                (current-column))
                               (continued-expr-p
                                (+ (current-column) (* 2 js-indent-level)
                                   js-expr-indent-offset))
                               (t
                                (+ (current-column) js-indent-level
                                   (pcase (char-after (nth 1 parse-status))
                                     (?\( js-paren-indent-offset)
                                     (?\[ js-square-indent-offset)
                                     (?\{ js-curly-indent-offset)))))))
                   (if in-switch-p
                       (+ indent js-switch-indent-offset)
                     indent))))
              ;; If there is something following the opening
              ;; paren/bracket, everything else should be indented at
              ;; the same level.
              (t
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column)))))

          ((ds/js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))


(defun ds/biosite-js--multi-line-declaration-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js--declaration-keyword-re))
        (when (looking-at js--indent-operator-re)
          (goto-char (match-end 0)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at js--indent-operator-re)
                                   (js--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (js--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js--declaration-keyword-re)
          (if ds/js-fancy-align
              (progn
                (goto-char (match-end 0))
                (1+ (current-column)))
            (progn
              ;; (forward-line -1)
              (back-to-indentation)
              (+ (current-column) js-indent-level))))))))


;; lol, more monkey patching
(fset #'js--proper-indentation #'ds/biosite-js--proper-indentation)
(fset #'js--multi-line-declaration-indentation #'ds/biosite-js--multi-line-declaration-indentation)

(fset #'typescript--proper-indentation #'ds/biosite-js--proper-indentation)
(fset #'typescript--multi-line-declaration-indentation #'ds/biosite-js--multi-line-declaration-indentation)




;; Activate all this stuff in a minor mode (eventually, for now only some
;; things...)

(define-minor-mode biosite-mode
  "Set up Biosite specific syntax etc."
  :lighter " Biosite"
  :global nil

  ;; body
  (ds/highlight-space-indents biosite-mode)

  (set 'indent-tabs-mode biosite-mode)

  (if biosite-mode
      (progn

        (setq-local fill-column 100)

        (add-hook 'sql-mode-hook #'ds/biosite-set-postgres nil t)
        (when (derived-mode-p 'sql-mode)
          (ds/biosite-set-postgres))

        (when (derived-mode-p 'c++-mode)
          (local-set-key (kbd "C-,") #'ds/biosite-include)
          (local-set-key (kbd "C-_") #'ds/biosite-insert-current-namespace))

        (when (derived-mode-p 'js-mode)
          (setq-local js-switch-indent-offset 0)
          (setq-local ds/js-indent-chain nil))

        (when (derived-mode-p 'sass-mode)
          (setq-local sass-indent-offset 4)))
    (remove-hook 'sql-mode-hook #'ds/biosite-set-postgres t)))

(defun maybe-enable-biosite-mode ()
  (interactive)
  (if (-any?
       (lambda (project-regex) (s-match project-regex (projectile-project-name)))
       '(".*boron.*" ".*caesium.*"))
      (biosite-mode 1)
    (biosite-mode 0)))

(add-hook 'c++-mode-hook #'maybe-enable-biosite-mode)
(add-hook 'js-mode-hook #'maybe-enable-biosite-mode)
(add-hook 'html-mode-hook #'maybe-enable-biosite-mode)
(add-hook 'css-mode-hook #'maybe-enable-biosite-mode)
(add-hook 'sass-mode-hook #'maybe-enable-biosite-mode)

(defun ds/biosite-set-postgres ()
  (sql-set-product "postgres"))




(defun ds/sort-headers--first-include ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "#include")
    (point-at-bol)))

(defun ds/sort-headers--last-include ()
  (save-excursion
    (goto-char (point-max))
    (search-backward "#include")
    (point-at-eol)))


(makunbound 'ds/sort-headers-external-libs)
(defcustom ds/sort-headers-external-libs
  '("nowarnings/" "boost/" "Q")
  "")

(makunbound 'ds/sort-headers-internal-libs)
(defcustom ds/sort-headers-internal-libs
  '("common/" "db/" "json/" "serialise2/" "compiler/" "rest/" "https/"
    "network/" "ssl/" "crypt/" "log/" "io/" "paths/" "options/"
    "process/" "qtlib/" "auth/" "version/"
    )
  "")

(makunbound 'ds/sort-headers-local-libs)
(defcustom ds/sort-headers-local-libs
  '("boron/" "xenon/" "shared/")
  "")

(makunbound 'ds/sort-headers-stdlibs)
(defcustom ds/sort-headers-stdlibs
  '("cstdlib" "csignal" "csetjmp" "cstdarg" "typeinfo" "typeindex" "type_traits" "bitset"
    "functional" "utility" "ctime" "chrono" "cstddef" "initializer_list" "tuple" "any"
    "optional" "variant" "new" "memory" "scoped_allocator" "memory_resource" "climits"
    "cfloat" "cstdint" "cinttypes" "limits" "exception" "stdexcept" "cassert" "system_error"
    "cerrno" "cctype" "cwctype" "cstring" "cwchar" "cuchar" "string" "string_view" "array"
    "vector" "deque" "list" "forward_list" "set" "map" "unordered_set" "unordered_map"
    "stack" "queue" "algorithm" "execution" "iterator" "cmath" "complex" "valarray"
    "random" "numeric" "ratio" "cfenv" "iosfwd" "ios" "istream" "ostream" "iostream"
    "fstream" "sstream" "strstream" "iomanip" "streambuf" "cstdio" "locale" "clocale"
    "codecvt" "regex" "atomic" "thread" "mutex" "shared_mutex" "future" "condition_variable"
    "filesystem" "experimental/algorithm" "experimental/any" "experimental/chrono" "experimental/deque"
    "experimental/execution_policy" "experimental/exception_list" "experimental/filesystem"
    "experimental/forward_list" "experimental/future" "experimental/list" "experimental/functional"
    "experimental/map" "experimental/memory" "experimental/memory_resource" "experimental/numeric"
    "experimental/optional" "experimental/ratio" "experimental/regex" "experimental/set"
    "experimental/string" "experimental/string_view" "experimental/system_error" "experimental/tuple"
    "experimental/type_traits" "experimental/unordered_map" "experimental/unordered_set"
    "experimental/utility" "experimental/vector" "cassert" "cctype" "cerrno" "cfenv" "cfloat"
    "cinttypes" "climits" "clocale" "cmath" "csetjmp" "csignal" "cstdarg" "cstddef" "cstdint"
    "cstdio" "cstdlib" "cstring" "ctime" "cuchar" "cwchar" "cwctype" "ccomplex" "complex"
    "complex" "ctgmath" "complex" "cmath" "ctgmath" "ciso646" "ciso646"
    "cstdalign" "cstdalign" "cstdbool" "cstdbool")
  "")


(defun ds/sort-headers--is-this-files-header (header)
  (-is-suffix? (f-split (f-no-ext header))
               (f-split (f-no-ext (buffer-file-name)))))


(defun ds/sort-headers--classify (header-line)
  (let ((normalised-header (->> header-line
                                (s-chop-prefix "#include")
                                (s-trim)
                                (s-chop-prefixes '("\"" "<"))
                                (s-chop-suffixes '("\"" ">")))))
    (cond
     ((s-starts-with? "catch" normalised-header) -10)
     ((ds/sort-headers--is-this-files-header normalised-header) 0)
     ((--any? (s-equals? it normalised-header) ds/sort-headers-stdlibs) 5)
     ((--any? (s-starts-with? it normalised-header) ds/sort-headers-external-libs) 10)
     ((--any? (s-starts-with? it normalised-header) ds/sort-headers-internal-libs) 20)
     ((--any? (s-starts-with? it normalised-header) ds/sort-headers-local-libs) 30)
     (t 40))))

(defun ds/sort-headers--internal (header-text)
  (->> header-text
       (s-split "\n")
       (-map #'s-trim)
       (--filter (not (string-empty-p it)))
       (-group-by #'ds/sort-headers--classify)
       (--sort (< (car it) (car other)))
       (map-values)
       (--map (-sort #'string-lessp it))
       (--map (s-join "\n" it))
       (s-join "\n\n")))


(defun ds/sort-headers ()
  (interactive)
  (let* ((start (ds/sort-headers--first-include))
         (end (ds/sort-headers--last-include))
         (out (ds/sort-headers--internal (buffer-substring start end))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert out))))


