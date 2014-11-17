
(require 'eclim)
(global-eclim-mode)


(require 'eclimd)

(set 'eclim-eclipse-dirs '("~/code/eclipse"))
(set 'eclim-executable "~/code/eclipse/eclim")
(set 'eclimd-wait-for-process nil)

;; don't think we can change this now :(
(set 'eclimd-workspace-dir "~/workspace")



(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(defun java-keys ()
  (interactive)
  (local-set-key (kbd "M-.") 'eclim-java-find-declaration)
  
  (local-set-key (kbd "C-c") '())

  ;; ;; Fix tab bindings (messed up by yas and/or binding C-i
  ;; (local-set-key (kbd "TAB") '())
  ;; (set-tab)

  )
(add-hook 'java-mode-hook 'java-keys)

;; Run eclimd automatically when we open a java buffer (if not running
;; already).
(add-hook 'java-mode-hook 
          (lambda () (if (not (eclimd--running-p))
                         (start-eclimd eclimd-workspace-dir))))

