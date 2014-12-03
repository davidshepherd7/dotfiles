
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

         (paths-to-try (-table-flat #'s-concat dirs-to-try files-to-try)))

    ;; Open the first file that exists
    (find-file (car (-filter #'file-exists-p paths-to-try)))))

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

         (paths-to-try (-table-flat #'s-concat dirs-to-try files-to-try)))

    ;; Open the first file that exists
    (find-file (car (-filter #'file-exists-p paths-to-try)))))


(defun java-keys ()
  (interactive)
  ;; (local-set-key (kbd "M-.") 'eclim-java-find-declaration)

  (local-set-key (kbd "C-c") '())
  (local-set-key (kbd "C-\\ o") #'java-toggle-test-file)

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

