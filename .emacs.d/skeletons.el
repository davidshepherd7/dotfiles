

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

(define-skeleton markdown-hilight
  "" nil
  "{% highlight scm %}" \n
  _ \n
  "{% endhighlight %}")

(define-skeleton int-magd
  "" nil
  "\\int_\\magd \\tbf " _ " \\d\\magd")

(define-skeleton int-bd
  "" nil
  "\\int_\\boundd \\tbf " _ " \\d\\boundd")

;; c++
;; ============================================================

(define-skeleton fuj-skele
  "" nil
  "for(unsigned j=0; j<"_"; j++)" > \n
  "{" > \n
  "" > \n
  "}" > \n)

(define-skeleton funj-skele
  "" nil
  "for(unsigned j=0, nj="_"; j<nj; j++)" > \n
  "{" > \n
  "" > \n
  "}" > \n)

(define-skeleton fui-skele
  "" nil
  "for(unsigned i=0; i<"_"; i++)" > \n
  "{" > \n
  "" > \n
  "}" > \n)

(define-skeleton funi-skele
  "" nil
  "for(unsigned i=0, ni="_"; i<ni; i++)" > \n
  "{" > \n
  "" > \n
  "}" > \n)

(define-skeleton fuk-skele
  "" nil
  "for(unsigned k=0; k<"_"; k++)" > \n
  "{" > \n
  "" > \n
  "}" > \n)

(define-skeleton funk-skele
  "" nil
  "for(unsigned k=0, nk="_"; k<nk; k++)" > \n
  "{" > \n
  "" > \n
  "}" > \n)

(define-skeleton sct-skele
  "" nil
  "std::cout << "_" << std::endl;" >)

(define-skeleton idperr-skele
  "" nil
  "#ifdef PARANOID" > \n
  "if("_")" > \n
  "{" > \n
  "throw OomphLibError(\"\"," > \n
  "OOMPH_EXCEPTION_LOCATION," > \n
  "OOMPH_CURRENT_FUNCTION);" > \n
  "}" > \n
  "#endif" > \n
  )


;; python
;; ============================================================

(define-skeleton pystart-skele
  "" nil
  "#! /usr/bin/env/python" \n
  \n
  "# Python 2/3 compatability" \n
  "from __future__ import print_function" \n
  "from __future__ import division" \n
  "from __future__ import absolute_import" \n
  \n
  "import sys" \n
  "import argparse" \n
  \n
  \n
  "def main():" \n
  _ \n
  "return 0" \n
  \n
  \n

  "if __name__ == \"__main__\":" \n
  "sys.exit(main())" \n)

(define-skeleton py-argparse-skele
  "" nil
  "# Parse arguments" \n
  "# ============================================================" \n
  "" \n
  "parser = argparse.ArgumentParser(description=main.__doc__," \n
  "" \n
  "# Don't mess up my formating in the help message" \n
  "formatter_class=argparse.RawDescriptionHelpFormatter)" \n
  \n
  "# parser.add_argument('--oomph-root', '-C', action = \"store\")" \n
  \n
  "args = parser.parse_args()" \n \n)
