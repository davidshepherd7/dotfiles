
(require 'sgml-mode)
(require 'js)
(require 'dash)
(require 's)
(require 'f)
(require 'projectile)

(require 'align)
(defun ds/disable-tabs-advice (&rest args)
  "Use spaces not tabs for advised function"
  (let ((indent-tabs-mode nil))
    (apply args)))
(advice-add #'align :around #'ds/disable-tabs-advice)
(advice-add #'align-regexp :around #'ds/disable-tabs-advice)
(advice-add #'align-region :around #'ds/disable-tabs-advice)
;; Add any other alignment functions here



(defun ds/comment-offset (dummy)
  "No offset for closing comment lines"
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-+\\*/") 0 '+)))

(defun ds/biosite-c-style ()
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

  ;; Indent comments by one tab normally, or none on closing comment lines.
  (c-set-offset 'c #'ds/comment-offset)
  )

(add-hook 'c++-mode-hook #'ds/biosite-c-style)


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

    (insert (format "UPDATE version SET current = %i;" next-version))))


;; html indentation
;; ============================================================

(defvar ds/sgml-fancy-align nil)

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
       (+ (current-indentation) sgml-basic-offset))))

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

(defvar ds/js-non-indented-line-regexs '("^\\s-*(function\\s-*()\\s-*{")
  "Lines matching this regex set the current indentation level to zero.")

(defun non-indented-line (line)
  (-any (lambda (reg) (string-match-p reg line))
        ds/js-non-indented-line-regexs))

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
                 (continued-expr-p  (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (cond
              ((non-indented-line (buffer-substring (point-at-bol) (point-at-eol)))
               0)
              ((or (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)") (not ds/js-fancy-align))
               (progn ; nothing following the opening paren/bracket
                 (message "a")
                 (skip-syntax-backward " ")
                 (when (eq (char-before) ?\)) (backward-list))
                 (back-to-indentation)
                 (let* ((in-switch-p (unless same-indent-p
                                       (looking-at "\\_<switch\\_>")))
                        (same-indent-p (or same-indent-p
                                           (and switch-keyword-p
                                                in-switch-p)))
                        (indent
                         (cond (same-indent-p
                                (current-column))
                               (continued-expr-p
                                (message "cont-expr")
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
               (message "b")
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column)))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

;; lol, more monkey patching
(fset #'js--proper-indentation #'ds/biosite-js--proper-indentation)
