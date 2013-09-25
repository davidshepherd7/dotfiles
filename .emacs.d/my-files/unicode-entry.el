
(defun set-unicode-keys ()
  "Bind some unicode symbols that I like to ctrl + alt + super +
... key combos."
  (interactive)

  ;; Some greek letters for maths
  (global-set-key (kbd "M-s-a") "𝛼")
  (global-set-key (kbd "M-s-b") "𝛽")
  (global-set-key (kbd "M-s-g") "𝛾")
  (global-set-key (kbd "M-s-d") "𝛿")
  (global-set-key (kbd "M-s-D") "𝛥")
  (global-set-key (kbd "M-s-e") "𝜀")
  (global-set-key (kbd "M-s-l") "λ")
  (global-set-key (kbd "M-s-L") "Λ")
  (global-set-key (kbd "M-s-p") "π")
  (global-set-key (kbd "M-s-s") "σ")
  (global-set-key (kbd "M-s-S") "Σ")
  (global-set-key (kbd "M-s-m") "𝜇")
  (global-set-key (kbd "M-s-f") "𝜙")
  (global-set-key (kbd "M-s-F") "𝛷")
  (global-set-key (kbd "M-s-y") "𝜓")
  (global-set-key (kbd "M-s-Y") "𝛹")
  (global-set-key (kbd "M-s-o") "𝜔")
  (global-set-key (kbd "M-s-O") "𝛺")
  ;; (global-set-key (kbd "M-s-P") "𝜁")
  (global-set-key (kbd "M-s-z") "𝜉")
  (global-set-key (kbd "M-s-n") "𝜂")


  ;; Some other maths symbols
  ;; (global-set-key (kbd "M-s-2") "") ;; can't get ^2 to work...
  ;; (global-set-key (kbd "M-s-3") "³")


  ;; Some accents
  (global-set-key (kbd "M-s-'") "́")
  (global-set-key (kbd "M-s-`") "̀")
  (global-set-key (kbd "M-s-^") "̆")
  (global-set-key (kbd "M-s-\"") "̈"))

(set-unicode-keys)
