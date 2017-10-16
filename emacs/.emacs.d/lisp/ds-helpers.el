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

(defun ds/print (x)
  "Print with newline TODO: multiple args"
  (pp x)
  (terpri))
