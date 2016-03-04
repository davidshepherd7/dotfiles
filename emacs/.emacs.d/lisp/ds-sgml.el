
(require 'sgml-mode)

(defun insert-br-tag ()
  (interactive)
  (insert "<br>"))
(define-key html-mode-map (kbd "M-RET") #'insert-br-tag)

(require 'aggressive-indent)
(add-hook 'sgml-mode-hook #'aggressive-indent-mode)

(defun ds/switch-to-js ()
  (interactive)
  (ds/switch-to-related ".js"))
(define-key html-mode-map (kbd "C-\\ o") #'ds/switch-to-js)


(defun ds/html-switch-to-test ()
  (interactive)
  (ds/switch-to-related ".spec.js"))
(define-key html-mode-map (kbd "C-\\ n") #'ds/html-switch-to-test)


(define-key sgml-mode-map (kbd "C-M-n") #'sgml-skip-tag-forward)
(define-key sgml-mode-map (kbd "C-M-e") #'sgml-skip-tag-backward)


;; Magic tag insertion
(define-key sgml-mode-map (kbd "<") #'sgml-tag)

;; Add some custom tags to the magic tag insertion
(defvar ds/angular-tags
  '(
    "ng-app"
    "ng-bind"
    "ng-repeat"
    "ng-view"

    ;; boron tags
    "edit-form-text"
    "edit-form-textarea"
    "edit-form-datepicker"
    "edit-form-boolean"
    "edit-form-select"
    "edit-form-number"
    "edit-form-time"
    "edit-form-time-duration"
    "edit-form-datetime"
    "edit-form-color"
    "edit-form-optional"
    "edit-form-custom"
    ))

(defun ds/add-angular-tags ()
  (interactive)
  (mapc (lambda (x) (add-to-list 'sgml-tag-alist x)) ds/angular-tags))
(add-hook 'html-mode-hook #'ds/add-angular-tags)


(require 'flycheck)
(add-hook 'html-mode-hook #'flycheck-mode)
(add-hook 'css-mode-hook #'flycheck-mode)
