
;; (require 'eclim)
;; (global-eclim-mode)


;; (require 'eclimd)

;; (set 'eclim-eclipse-dirs '("~/code/eclipse"))
;; (set 'eclim-executable "~/code/eclipse/eclim")
;; (set 'eclimd-wait-for-process nil)

;; ;; don't think we can change this now :(
;; (set 'eclimd-workspace-dir "~/.eclipse-workspace")



;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

(require 's)
(require 'dash)

(defun java-toggle-test-file ()
  "Try to locate the file containing tests for this file (or vice versa)."
  (interactive)
  (if (s-contains? "test" (buffer-file-name) 't)
      (java-switch-from-test)
    (java-switch-to-test)))

(defun java-switch-to-test ()
  (let* ((bf-dir (file-name-directory (buffer-file-name)))
         (bf-ext (s-concat "." (file-name-extension (buffer-file-name))))
         (bf-fnm (file-name-base (buffer-file-name)))

         (files-to-try
          (list (s-concat bf-fnm "Test" bf-ext)
                (s-concat "Test" bf-fnm bf-ext)))

         (dirs-to-try
          (-distinct (list bf-dir
                           (s-replace "src/" "test/" bf-dir)
                           (s-replace "source/" "test/" bf-dir))))

         (paths-to-try (-table-flat #'s-concat dirs-to-try files-to-try))

         (path (-first #'file-exists-p paths-to-try)))

    (when (not path) (throw 'no-test-file-found "No candidate paths found"))

    ;; Open the first file that exists
    (find-file path)))

(defun java-switch-from-test ()
  (let* ((bf-dir (file-name-directory (buffer-file-name)))
         (bf-ext (s-concat "." (file-name-extension (buffer-file-name))))
         (bf-fnm (file-name-base (buffer-file-name)))

         (files-to-try
          (list (s-replace "Test" "" (s-concat bf-fnm bf-ext))))

         (dirs-to-try
          (-distinct (list bf-dir
                           (s-replace "test/" "src/" bf-dir)
                           (s-replace "test/" "source/" bf-dir))))

         (paths-to-try (-table-flat #'s-concat dirs-to-try files-to-try))

         (path (-first #'file-exists-p paths-to-try)))

    (when (not path) (throw 'no-class-file-found "No candidate paths found"))

    ;; Open the first file that exists
    (find-file path)))


(defun java-keys ()
  (interactive)
  ;; (local-set-key (kbd "M-.") 'eclim-java-find-declaration)

  (local-set-key (kbd "C-c") '())
  (local-set-key (kbd "C-\\ o") #'java-toggle-test-file)
  (local-set-key (kbd "C-d") 'nil)


  ;; ;; Fix tab bindings (messed up by yas and/or binding C-i
  ;; (local-set-key (kbd "TAB") '())
  ;; (set-tab)

  )
(add-hook 'java-mode-hook 'java-keys)

;; ;; Run eclimd automatically when we open a java buffer (if not running
;; ;; already).
;; (add-hook 'java-mode-hook
;;           (lambda () (unless (eclimd--running-p)
;;                        (start-eclimd eclimd-workspace-dir)
;;                        (define-key java-mode-map (kbd "C-c") nil))))


(defun maybe-set-cws-style ()
  "Detect if we are editing cws code and set the style if so."
  (interactive)
  (when (or (equal (projectile-project-name) "cloudworkflowsimulator")
            (equal (projectile-project-name) "driver"))
    (c-set-style "java")
    (c-set-offset 'statement-cont '++)
    (c-set-offset 'arglist-cont-nonempty '++)
    (c-set-offset 'func-decl-cont '++)
    (c-set-offset 'arglist-intro '++)))
(add-hook 'java-mode-hook 'maybe-set-cws-style)

(defun java-package-name ()
  (let* ((root-regex "src/\\|source/\\|test/")
         (path-from-src (-last-item
                         (s-split root-regex (file-name-directory (buffer-file-name))))))
    (s-replace "/" "." (s-chop-suffix "/" path-from-src))))

(defun java-insert-package-name ()
  (interactive)
  (insert (java-package-name)))

(defun java-insert-import-util ()
  (insert "import java.util.List;
  import java.util.ArrayList;
  import java.util.Map;
  import java.util.HashMap;
  import java.util.TreeMap;
  import java.util.Set;
  import java.util.HashSet;
  import java.util.SortedSet;
  import java.util.Collection;
  import static java.util.Arrays.asList;
  import static java.util.Collections.sort;
  import static java.util.Collections.reverse;
  import static java.util.Collections.unmodifiableCollection;"))
