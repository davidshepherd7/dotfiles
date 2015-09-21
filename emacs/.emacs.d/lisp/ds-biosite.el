

(defun biosite-c-style ()
  (interactive)

  ;; Ensure that one offset is a tab
  (set 'c-basic-offset tab-width)

  (c-set-offset 'substatement-open '0)

  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-close '++))

(defun ds/tabs-mode ()
  (interactive)
  (set 'indent-tabs-mode t))

(add-hook 'c++-mode-hook #'biosite-c-style)
(add-hook 'c++-mode-hook #'ds/tabs-mode)
