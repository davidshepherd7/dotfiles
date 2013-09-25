;; Emacs options and keys for C++

;; Set .h files to use c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun indent-buffer ()
  "Re-indent the whole buffer"
  (interactive)
  ;; (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))


;; Save and compile with f5
(add-hook 'c-mode-common-hook 'my-c-mode-keys)

(defun my-c-mode-keys ()
  (use-local-map '())
  (local-set-key (kbd "<f5>") 'my-recompile)
  (local-set-key (kbd "C-`") 'next-error)
  (local-set-key (kbd "C-¬") 'previous-error)
  (local-set-key (kbd "C-\\ o") 'ff-find-other-file))

(defun cpp-access-function ()
  "Create set and get access functions for the selected member
variable. Cannot deal with keywords like static or const. These
access functions are BAD for class access (too much copying)."
  (let* ((var-string
          (replace-regexp-in-string ";" "" (buffer-substring (region-beginning)
                                                             (region-end))))
         (var-string-list (split-string var-string))
         (var-type (car var-string-list))
         (var-name (cadr var-string-list)))
    (concat
     (format "/// \\short Non-const access function for %s.\n" var-name)
     (format "%s& %s() {return %s;}\n\n" var-type (downcase var-name) var-name)
     (format "/// \\short Const access function for %s.\n" var-name)
     (format "%s %s() const {return %s;}\n\n" var-type (downcase var-name) var-name))))

(defun cpp-access-function-kill-ring ()
  "Add access functions for selected member variable to kill ring."
  (interactive)
  (kill-new (cpp-access-function)))
