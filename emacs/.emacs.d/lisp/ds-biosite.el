

(defun biosite-c-style ()
  (interactive)

  ;; Ensure that one offset is a tab
  (set 'c-basic-offset tab-width)

  (c-set-offset 'substatement-open '0)

  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '+))

(defun ds/tabs-mode ()
  (interactive)
  (set 'indent-tabs-mode t))

(add-hook 'c++-mode-hook #'biosite-c-style)
(add-hook 'c++-mode-hook #'ds/tabs-mode)

(defun ds/biosite-html-style ()
  (interactive)
  (setq sgml-basic-offset tab-width)
  (setq indent-tabs-mode t)
  (setq tab-stop-list '(4 8 12 16)))

(add-hook 'html-mode-hook #'ds/biosite-html-style)

(defun ds/biosite-js-style ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq js-indent-level tab-width)
  (setq js0comment-)
  )
