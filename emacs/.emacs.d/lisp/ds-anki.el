(require 'markdown-mode)

(define-derived-mode anki-mode markdown-mode
  "Anki")

(define-key anki-mode-map (kbd "C-c C-c") #'anki-mode-send-and-new-card)
(define-key anki-mode-map (kbd "$") #'anki-mode-insert-latex-math)

(defun anki-mode-send-and-new-card ()
  (interactive)
  (anki-mode-send-new-card)
  (anki-mode-new-card))

(defun anki-mode-insert-latex-math ()
  (interactive)
  ;; TODO: handle region
  (insert "[$][/$]")
  (forward-char -4))

;;;###autoload
(defun anki-mode-new-card ()
  (interactive)
  (find-file (make-temp-file "anki-card-"))
  (anki-mode)

  (insert "@Front\n\n")
  (insert "@Back\n")
  (forward-line -2))

(defun anki-mode-send-new-card ()
  (interactive)
  (let ((default-directory "~/code/ankicli")
        (is-cloze (not (equal 0 (anki-mode--max-cloze)))))
    (when (not (equal 0
                      (apply #'call-process
                             "~/code/ankicli/bin/anki-cli" (buffer-file-name) t t
                             (-concat '("--markdown")
                                      (if is-cloze
                                          '("--model" "Cloze")
                                        '("--model" "Basic (and reversed card)"))
                                      ))))
      (error "anik-cli failed"))))


(defun anki-mode--max-cloze ()
  (--> (buffer-substring-no-properties (point-min) (point-max))
       (s-match-strings-all "{{c\\([0-9]\\)::" it)
       (-map #'cadr it) ; First group of each match
       (-map #'string-to-number it)
       (or it '(0))
       (-max it)))

(defun anki-mode-cloze-region (start end)
  (interactive "r")
  (save-excursion
    ;; Do end first because inserting at start moves end
    (goto-char end)
    (insert "}}")

    (goto-char start)
    (insert "{{c")
    (insert (number-to-string (1+ (anki-mode--max-cloze))))
    (insert "::")
    ))

(provide 'anki-mode)
