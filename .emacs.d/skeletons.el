
;; Don't let skeletons trigger abbreviations
(setq skeleton-further-elements '((abbrev-mode nil)))


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

(define-skeleton latex-figure
  "" nil
  "\\includegraphics[width=0.8\\textwidth]{\"./"_"\"}" > \n
  )


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

(define-skeleton nbr
  "" nil
  \n
  "{" > \n
  _ \n
  "}" > \n
  )

;; oomph-lib
;; ============================================================

(define-skeleton idperr-skele
  "" nil
  "#ifdef PARANOID" > \n
  "if("_")" > \n
  "{" > \n
  "std::string err = \"\";" > \n
  "throw OomphLibError(err, OOMPH_CURRENT_FUNCTION," > \n
  "OOMPH_EXCEPTION_LOCATION);" > \n
  "}" > \n
  "#endif" > \n
  )

(define-skeleton ifdef-hlib-skele
  "" nil
  "#ifdef OOMPH_HAS_HLIB" > \n
  _ \n
  "#endif" > \n
  )

(define-skeleton ifdef-paranoid-skele
  "" nil
  "#ifdef PARANOID" > \n
  _ \n
  "#endif" > \n
  )

(define-skeleton ifdef-mpi-skele
  "" nil
  "#ifdef OOMPH_HAS_MPI" > \n
  _ \n
  "#endif" > \n
  )

(define-skeleton oomph-err-skele
  "" nil
  "std::string err = \"" _ "\";" > \n
  "throw OomphLibError(err, OOMPH_CURRENT_FUNCTION," > \n
  "OOMPH_EXCEPTION_LOCATION);" > \n
  )

(define-skeleton oomph-warn-skele
  "" nil
  "std::string err = \"" _ "\";" > \n
  "throw OomphLibWarning(err, OOMPH_CURRENT_FUNCTION," > \n
  "OOMPH_EXCEPTION_LOCATION);" > \n
  )

(define-skeleton oomph-fnode-skele
  "" nil
  "for(unsigned nd=0, nnd=ele_pt->nnode(); nd<nnd; nd++)" > \n
  "{" > \n
  "Node* nd_pt = ele_pt->node_pt(nd);" > \n
  "nd_pt->" _ ";" > \n
  "}" > \n
  )

(define-skeleton oomph-fnode-mesh-skele
  "" nil
  "for(unsigned nd=0, nnd=mesh_pt->nnode(); nd<nnd; nd++)" > \n
  "{" > \n
  "Node* nd_pt = mesh_pt->node_pt(nd);" > \n
  "nd_pt->" _ ";" > \n
  "}" > \n
  )

(define-skeleton oomph-fele-skele
  "" nil
  "for(unsigned ele=0, nele=mesh_pt->nelement(); ele<nele; ele++)" > \n
  "{" > \n
  "FiniteElement* ele_pt = mesh_pt->finite_element_pt(ele);" > \n
  _ > \n
  "}" > \n
  )

(define-skeleton oomph-fmesh-skele
  "" nil
  "// Loop over all meshes in problem" > \n
  "for(unsigned msh=0, nmsh=nsub_mesh(); msh<nmsh; msh++)" > \n
  "{" > \n
  "mesh_pt(msh)->"_";" > \n
  "}" > \n
  )


(define-skeleton oomph-fbulk-mesh-skele
  "" nil
  "// Loop over all bulk meshes in problem" > \n
  "for(unsigned msh=0, nmsh=nsub_mesh(); msh<nmsh; msh++)" > \n
  "{" > \n
  "// Skip non-bulk meshes" > \n
  "if(mesh_pt(msh)->node_pt(0)->ndim() != Dim) continue;" > \n
  "" > \n
  "mesh_pt(msh)->"_";" > \n
  "}" > \n
  )

(define-skeleton oomph-not-implemented-error
  "" nil
  "throw OomphLibError(\"Function not yet implemented\"," > \n
  "OOMPH_CURRENT_FUNCTION, OOMPH_EXCEPTION_LOCATION);" > \n
  )

;; python
;; ============================================================

(define-skeleton pystart-skele
  "" nil
  "#!/usr/bin/env python" \n
  \n
  "# Python 2/3 compatability" \n
  "from __future__ import print_function" \n
  "from __future__ import division" \n
  "from __future__ import absolute_import" \n
  \n
  "import sys" \n
  "import argparse" \n
  "import os" \n
  "import os.path" \n
  \n
  "from os.path import join as pjoin" \n
  \n
  "def main():" \n
  _ \n
  "return 0" \n
  \n
  \n

  "if __name__ == \"__main__\":" \n
  "sys.exit(main())" \n)


(define-skeleton oomph-pystart-skele
  "" nil
  "#!/usr/bin/env python" \n
  \n
  "# Python 2/3 compatability" \n
  "from __future__ import print_function" \n
  "from __future__ import division" \n
  "from __future__ import absolute_import" \n
  \n
  "import sys" \n
  "import argparse" \n
  "import os" \n
  "import os.path" \n
  \n
  "from os.path import join as pjoin" \n
  \n
  "# Make sure *this* versions oomphpy is in the path (before any other" \n
  "# versions in other places)" \n
  "sys.path.insert(1, pjoin(os.path.dirname(__file__), \"../etc\"))" \n
  "import oomphpy" \n
  "import oomphpy.micromagnetics as mm" \n
  \n
  "def main():" \n
  _ \n
  "return 0" \n
  \n
  \n

  "if __name__ == \"__main__\":" \n
  "sys.exit(main())" \n)


(define-skeleton pyargparse-skele
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

;; c
;; ============================================================

(define-skeleton skeleton-skele
  "" nil
  "(define-skeleton " _"-skele" \n
  "\"\" nil" > \n
  \n
  ")" > \n
  )

(define-skeleton print-int-skele
  "" nil
  "printf(\"%d\\n\", "_")" > \n
  )

(define-skeleton print-float-skele
  "" nil
  "printf(\"%f\n\", "_")" > \n
  )
