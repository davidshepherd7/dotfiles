
(require 'markdown-mode)

(defun ds-anki-new-card ()
  (interactive)
  (find-file (make-temp-file "anki-card-"))
  (markdown-mode)
  (insert "@Front\n\n")
  (insert "@Back\n")
  (forward-line -2)

  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (ds-send-anki-new-card)
                                   (ds-anki-new-card))))

(defun ds-send-anki-new-card ()
  (interactive)
  (let ((default-directory "~/code/ankicli"))
    (when (not (equal
                (call-process "~/code/ankicli/bin/anki-cli" (buffer-file-name) t t "--markdown")
                0))
      (error "anik-cli failed"))))
