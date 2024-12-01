;; Emacs options and keys for C++
(require 'cc-mode)
(require 's)

(add-hook 'c-mode-hook #'set-tab)

;; Don't auto align macro newline escapes, it's not always consistent with how
;; other people write them
(validate-setq c-auto-align-backslashes nil)

;; Clear existing keys
(validate-setq c++-mode-map (make-sparse-keymap))

;; Set .h files to use c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; .ipp is some crazy boost thing for something like .cpp files that they want
;; to #include
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(defun indent-buffer ()
  "Re-indent the whole buffer"
  (interactive)
  ;; (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

;; ;; Endings to look for with find other file. First entry will be used if we
;; ;; are creating it.
;; (setq cc-other-file-alist
;;       (quote
;;        (("\\.cc\\'" (".hh" ".h"))
;;         ("\\.hh\\'" (".cc" ".C"))
;;         ("\\.c\\'" (".h"))
;;         ("\\.h\\'" (".cpp" ".cc" ".c" ".C" ".CC" ".cxx"))
;;         ("\\.C\\'" (".H" ".hh" ".h"))
;;         ("\\.H\\'" (".C" ".CC"))
;;         ("\\.CC\\'" (".HH" ".H" ".hh" ".h"))
;;         ("\\.HH\\'" (".CC"))
;;         ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
;;         ("\\.h\\+\\+\\'" (".c++"))
;;         ("\\.cpp\\'" (".hpp" ".hh" ".h"))
;;         ("\\.hpp\\'" (".cpp"))
;;         ("\\.cxx\\'" (".hxx" ".hh" ".h"))
;;         ("\\.hxx\\'" (".cxx")))))

;; Don't try to open includes etc (it never works...)
(validate-setq ff-special-constructs nil)

;; ;; Bind it
;; (define-key c++-mode-map (kbd "C-\\ o") #'ff-find-other-file)
;; (define-key c++-mode-map (kbd "C-\\ C-o") #'ff-find-other-file)

(defun ds/switch-to-from-header ()
  (interactive)
  (cond
   ((s-ends-with? ".h" (buffer-file-name))
    (find-file (s-concat (s-chop-suffix ".h" (buffer-file-name)) ".cpp")))
   ((s-ends-with? ".cpp" (buffer-file-name))
    (find-file (s-concat (s-chop-suffix ".cpp" (buffer-file-name)) ".h")))
   (t (error "Not a .cpp or .h file."))))
(define-key c++-mode-map (kbd "C-\\ o") #'ds/switch-to-from-header)
(define-key c++-mode-map (kbd "C-\\ i") #'ds/switch-to-from-header) ;; less rsi-inducing
(define-key c++-mode-map (kbd "C-\\ C-o") #'ds/switch-to-from-header)


;; For use in snippets
(defun cpp-to-h-path (cpp-path)
  (replace-regexp-in-string "\\.cpp" ".h" cpp-path))


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


(require 'company)
(defun ds/set-up-completion ()
  (interactive)
  (set (make-local-variable 'company-backends)
       (list ds/global-company-backends)))

(add-hook 'c++-mode-hook #'ds/set-up-completion)


;; Parse cppcheck output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(cppcheck "\\[\\([^]]*\\):\\([0-9]+\\)\\]:" 1 2))
(add-to-list 'compilation-error-regexp-alist 'cppcheck)


;; Parse boost test REQUIRE macro output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(boost-test-require "^\\(.*\\)(\\([0-9]*\\)): fatal error" 1 2))
(add-to-list 'compilation-error-regexp-alist 'boost-test-require)


;; Don't go so nuts with font-lock, it's slow
(defun ds/limit-font-lock ()
  (interactive)
  (setq-local font-lock-maximum-decoration 3))
(add-hook 'c++-mode-hook #'ds/limit-font-lock)

(require 'flycheck)
(defun ds/cpp-flycheck ()
  (interactive)
  (flycheck-mode)
  (flycheck-select-checker 'c/c++-cppcheck)
  (add-to-list 'flycheck-cppcheck-checks "warning")
  (add-to-list 'flycheck-cppcheck-checks "performance")
  (add-to-list 'flycheck-cppcheck-checks "information")
  (add-to-list 'flycheck-cppcheck-checks "missingInclude")
  )
(add-hook 'c++-mode-hook #'ds/cpp-flycheck)

(use-package modern-cpp-font-lock
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


(defun ds/cmake ()
  "Run cmake in a way that doesn't interfere with other compiles"
  (interactive)
  (let ((default-directory (or (projectile-compilation-dir) (projectile-project-root)))
        (previous-compile-command compile-command)
        (compilation-buffer-name-function (lambda (_arg) "*cmake*")))
    (compile "cmake .")
    (setq compile-command previous-compile-command)))
