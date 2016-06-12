;; -*- lexical-binding: t -*-

(require 'dash)
(require 'names)

;;;###autoload
(define-namespace ds/toggle-electricity-

:functionlike-macros (-->)

(defcustom electric-modes
  '(electric-operator-mode electric-indent-mode aggressive-fill-paragraph-mode aggressive-indent-mode)
  "Modes to toggle"
  :group 'electricity)

(defvar -modes-to-enable
  nil
  "List of modes which will be enabled on next call to toggle")

(defun -active-electric-modes ()
  (-filter #'symbol-value electric-modes))

(defun -disable ()
  (setq -modes-to-enable
        (-active-electric-modes))
  (--> -modes-to-enable
       (-map #'symbol-function it)
       (-each it (lambda (mode-fn) (funcall mode-fn 0))))
  (message "electricity disabled"))

(defun -enable ()
  (--> -modes-to-enable
       (-map #'symbol-function it)
       (-each it (lambda (mode-fn) (funcall mode-fn 1))))
  (setq -modes-to-enable
        nil)
  (message "electricity enabled"))

:autoload
(defun toggle ()
  "Toggle the modes in the list ds/toggle-electricity-electric-modes"
  (interactive)
  (if -modes-to-enable
      (-enable)
    (-disable)))

)

(provide 'ds/toggle-electricity)
