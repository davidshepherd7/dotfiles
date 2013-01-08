

;; Scheme skeletons
;; ============================================================
(define-skeleton scheme-stream-output
  "Insert output code for scheme streams" nil
  "stream-head " _ " 10)" > \n)



;; Latex skeletons
;; ============================================================

(define-skeleton latex-two-columns
  "Insert two columns each roughly half text width" nil
  "\\begin{columns}[c]" > \n \n
  "\\column{0.47\\textwidth}" > \n
  > _ \n \n
  "\\column{0.47\\textwidth}" > \n \n
  "\\end{columns}" >)
