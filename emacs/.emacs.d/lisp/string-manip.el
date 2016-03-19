(defun ds/buffer-basename ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
