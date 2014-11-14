
(defun csvtolatex ()
  (interactive)
  (setq cols 6)

  ;; Remove whitespace
  (beginning-of-buffer)
  (replace-regexp "[\t '\"]*" "")

  ;; Remove commas at end of lines and insert "\cline{...}"
  (beginning-of-buffer)
  (replace-regexp ",*\n" " \\\\\\\\ \n \\\\cline{2-6} \n")
  ;; (replace-regexp "number-of-columns" (format "%d" cols)) ;; hack...

  ;; Replace remaining commas with &
  (beginning-of-buffer)
  (replace-regexp "," " & ")

  ;; Replace sim and str by multicolumn code
  (end-of-buffer)
  (re-search-backward "^Sim" 1 t 0) ;; replace first occurance
  (replace-match "\\\\hline \\\\multicolumn{1}{|c|}{\\\\multirow{3}{*}{Sim}}")

  (end-of-buffer)
  (re-search-backward "^Str" 1 t 1)
  (replace-match "\\\\hline \\\\multicolumn{1}{|c|}{\\\\multirow{3}{*}{Str}}")

  (beginning-of-buffer)
  (replace-regexp "^Sim" "")
  (beginning-of-buffer)
  (replace-regexp "^Str" "")


  ;; Header
  (beginning-of-buffer)
  (insert "\\documentclass{article}
\\usepackage{multirow}
\\begin{document}
\\begin{table}[ht]
  \\begin{center}
    \\begin{tabular}{|")
  (dotimes (i cols) (insert "c|"))
  (insert "}\n\n")

  ;; Table top line
  (insert (format "\\cline{3-%d}\n" cols))
  (dotimes (i (- cols 4)) (insert "\\multicolumn{1}{c}{} &"))
  (insert (format "\\multicolumn{4}{|c|}{Number of equations} \\\\ \n \\cline{1-%d}" cols))

  ;; Footer
  (end-of-buffer)
(insert "\\hline
\\end{tabular}
  \\end{center}
  \\end{table}
  \\end{document}"))