
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
                                   (ds-anki-send-new-card)
                                   (ds-anki-new-card))))

(defun ds-anki-send-new-card ()
  (interactive)
  (let ((default-directory "~/code/ankicli")
        (is-cloze (not (equal 0 (ds-anki-max-cloze)))))
    (when (not (equal 0
                      (apply #'call-process
                             "~/code/ankicli/bin/anki-cli" (buffer-file-name) t t
                             (-concat '("--markdown")
                                      (when is-cloze '("--model" "Cloze"))))))
      (error "anik-cli failed"))))


(defun ds-anki-max-cloze ()
  (--> (buffer-substring-no-properties (point-min) (point-max))
       (s-match-strings-all "{{c\\([0-9]\\)::" it)
       (-map #'cadr it) ; First group of each match
       (-map #'string-to-number it)
       (or it '(0))
       (-max it)))

(defun ds-anki-cloze-region (start end)
  (interactive "r")
  (save-excursion
    ;; Do end first because inserting at start moves end
    (goto-char end)
    (insert "}}")

    (goto-char start)
    (insert "{{c")
    (insert (number-to-string (1+ (ds-anki-max-cloze))))
    (insert "::")
    ))
