
(require 'company)

(defun ds/set-up-cmake-completion ()
  (interactive)
  (set (make-local-variable 'company-backends)
       (list (list 'company-cmake)))
  (set (make-local-variable 'company-require-match)
       nil))
(add-hook 'cmake-mode-hook #'ds/set-up-cmake-completion)
