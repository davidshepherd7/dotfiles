;;; -*- lexical-binding: t -*-

(require 's)
(require 'misc) ; for forward-to-word



;;; Helpers

(defun s-interactive-operate-on-region (func beg end)
  (let ((initial (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (funcall func initial))))

(defun s-interactive-operate-on-word (func)
  ;; TODO: this eats punctuation if called between words
  (s-interactive-operate-on-region func (point) (save-excursion (forward-symbol 1) (point)))
  (forward-to-word 1))

(defun s-interactive-dwim-word-or-region (func)
  (if (use-region-p)
      (s-interactive-operate-on-region func (region-beginning) (region-end))
    (s-interactive-operate-on-word func)))

(defun s-interactive-upper-camel-case ()
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-upper-camel-case))




;;; Commands

(defun s-interactive-s-upcase()
  "Run s-upcase interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-upcase))

(defun s-interactive-s-downcase()
  "Run s-downcase interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-downcase))

(defun s-interactive-s-split-words()
  "Run s-split-words interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-split-words))

(defun s-interactive-s-lower-camel-case ()
  "Run s-lower-camel-case interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-lower-camel-case))

(defun s-interactive-s-upper-camel-case()
  "Run s-upper-camel-case interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-upper-camel-case))

(defun s-interactive-s-snake-case()
  "Run s-snake-case interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-snake-case))

(defun s-interactive-s-dashed-words()
  "Run s-dashed-words interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-dashed-words))

(defun s-interactive-s-capitalized-words()
  "Run s-capitalized-words interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-capitalized-words))

(defun s-interactive-s-titleized-words()
  "Run s-titleized-words interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-titleized-words))

(defun s-interactive-s-word-initials()
  "Run s-word-initials interactively

See `s-interactive-dwim-word-or-region'."
  (interactive "*")
  (s-interactive-dwim-word-or-region #'s-word-initials))
