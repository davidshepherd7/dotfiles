;;; pp-debug.el --- Pretty-print debug frames  -*- lexical-binding: t -*-


;; https://gist.githubusercontent.com/felipeochoa/c41ddd8d7cbee75b8b4ce0838ed8fb57/raw/aeefb7d63b8bcc0e2e3606b2142b1828e0a1b942/pp-debug.el

;; Copyright (C) 2017 Felipe Ochoa

;; Author: Felipe Ochoa
;; Created: 5 Dec 2017
;; License: GPL

;;; Commentary:
;;; Pretty-print debugger frames.

;;; Code:

(require 'debug)

(defvar pp-debug-frame nil)

(defun pp-debug-from-debugger ()
  "Open a new pp-debug buffer for the debugger frame at point."
  (interactive)
  (pp-debug (backtrace-frame (1- (debugger-frame-number)))))

(defun pp-debug (frame)
  "Pop open a pp-debug buffer for FRAME."
  (setq pp-debug-frame frame)
  (set-buffer (pop-to-buffer "*BT: Frame*"))
  (let ((inhibit-read-only t))
    (cl-destructuring-bind (special fn &rest args) pp-debug-frame
      (erase-buffer)
      (pp-debug-mode)
      (progn
        (if special (insert ";; special form"))
        (insert "(" (pp-to-string fn))
        (dolist (arg args)
          (insert "\n" (pp-to-string arg)))
        (insert ")"))
      (goto-char (point-min))
      (indent-pp-sexp))))

(defvar pp-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'pp-debug-up)
    (define-key map "d" 'pp-debug-down)
    (define-key map "s" 'debugger-step-through)
    (define-key map "c" 'debugger-continue)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map))

(define-derived-mode pp-debug-mode special-mode "Pretty Debugger"
  "Debug emacs in a pretty way."
  (lisp-mode-variables nil nil 'elisp))

(defun pp-debug-up ()
  "Re-render pp-debug with the frame above the current one."
  (interactive)
  (unless pp-debug-frame (error "No current pp-debug frame"))
  (let ((i 0) frame)
    (while (and (setq frame (backtrace-frame i))
                (not (equal frame pp-debug-frame)))
      (cl-incf i))
    (if frame
        (if (setq frame (backtrace-frame (1+ i)))
            (pp-debug frame)
          (message "Already at top-most frame"))
      (message "Current frame not found"))))

(defun pp-debug-down ()
  "Re-render pp-debug with the frame below the current one."
  (interactive)
  (unless pp-debug-frame (error "No current pp-debug frame"))
  (let ((i 1) frame (prev (backtrace-frame 0)))
    (if (equal prev pp-debug-frame)
        (message "Already at the bottom frame")
      (while (and (setq frame (backtrace-frame i))
                  (not (equal frame pp-debug-frame)))
        (setq prev frame)
        (cl-incf i))
      (if frame (pp-debug prev)
        (message "Current frame not found")))))

(define-key debugger-mode-map "r" 'pp-debug-from-debugger)

(provide 'pp-debug)

;;; pp-debug.el ends here
