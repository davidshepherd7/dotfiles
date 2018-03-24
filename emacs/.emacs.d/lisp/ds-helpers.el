;;; Useful helpers for lisp programming


(defun ds/buffer-context (start end n)
  "Print the contents of the buffer between start and end with n characters of context"
  (let ((before (buffer-substring (max (point-min) (- start n)) start))
        (match (buffer-substring start end))
        (after (buffer-substring end (min (point-max) (+ end n)))))
    (concat before "|" match "|" after)))

(defun ds/match-context (n)
  "Print the current match with n characters of context"
  (ds/buffer-context (match-beginning 0) (match-end 0) n))

(defun ds/print (&rest args)
  "Python-like print function"
  (dolist (arg args) (prin1 arg) (princ " "))
  (terpri))

(defun ds/print-buffer ()
  "Print the current buffer with a | indicating point"
  (princ (buffer-substring-no-properties (point-min) (point)))
  (princ "|")
  (princ (buffer-substring-no-properties (point) (point-max)))
  (terpri))
