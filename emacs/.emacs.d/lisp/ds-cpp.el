;; Emacs options and keys for C++

;; Set .h files to use c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun indent-buffer ()
  "Re-indent the whole buffer"
  (interactive)
  ;; (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

;; Endings to look for with find other file. First entry will be used if we
;; are creating it.
(setq cc-other-file-alist
      (quote
       (("\\.cc\\'" (".hh" ".h"))
        ("\\.hh\\'" (".cc" ".C"))
        ("\\.c\\'" (".h"))
        ("\\.h\\'" (".cpp" ".cc" ".c" ".C" ".CC" ".cxx"))
        ("\\.C\\'" (".H" ".hh" ".h"))
        ("\\.H\\'" (".C" ".CC"))
        ("\\.CC\\'" (".HH" ".H" ".hh" ".h"))
        ("\\.HH\\'" (".CC"))
        ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
        ("\\.h\\+\\+\\'" (".c++"))
        ("\\.cpp\\'" (".hpp" ".hh" ".h"))
        ("\\.hpp\\'" (".cpp"))
        ("\\.cxx\\'" (".hxx" ".hh" ".h"))
        ("\\.hxx\\'" (".cxx")))))

(defun my-c-mode-keys ()
  (use-local-map '())
  (local-set-key (kbd "C-\\ o") 'ff-find-other-file)
  (local-set-key (kbd "C-\\ C-o") 'ff-find-other-file)

  ;; (local-set-key [tab] 'yas-expand)
  ;; (set 'yas-fallback-behavior '(apply indent-according-to-mode))
  )
(add-hook 'c++-mode-hook 'my-c-mode-keys)


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

(defun auto-bracify ()
  "Find first if/for/... expression in region and make sure it has braces"
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (when (search-forward-regexp
     "\\(^[ \t]*\\)\\(else if\\|if\\|else\\|for\\|while\\)[ \t]*(.*)"
     (region-end) t)
      (newline-and-indent)
      (insert "{")
      (newline-and-indent) (end-of-line) (newline-and-indent)
      (insert "}"))))
