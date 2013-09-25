;; Emacs commands for use with oomph-lib

;; oomph-lib whitespace settings
;;=================================================================
(c-add-style "my-oomph-c-style"
             '("gnu"
               (c-basic-offset . 2)  ;; I don't like 1 space indents
               (c-offsets-alist (arglist-intro . ++))
               ;; Function arguments starting on the next line have a smaller indent
               ;;UnsteadyHeatProblem(
               ;;   UnsteadyHeatEquations<2>::UnsteadyHeatSourceFctPt source_fct_pt,
               ;;   FiniteElement::UnsteadyExactSolutionFctPt exact_solution_fct_pt,
               ;;   TimeStepper* ts_pt);
               ))

(c-add-style "strict-oomph-c-style"
             '("gnu"
               (c-basic-offset . 1)
               (indent-tabs-mode . nil)))

(defun is-oomph-code ()
  "Try to detect if we are working on oomph-lib's core or not."
  (interactive)
  (and buffer-file-name
       (string-match "oomph" buffer-file-name)
       (not (string-match "user_drivers" buffer-file-name))))

(defun is-oomph-user-driver-code ()
  "Try to detect if we are working on oomph-lib user drivers."
  (and buffer-file-name
       (string-match "oomph" buffer-file-name)
       (string-match "user_drivers" buffer-file-name)))

;; Switch to oomph-lib style indentation only if we are in a directory with
;; "oomph" in the path somewhere. If it'sa user drivers folder then I can
;; use my own indent levels.
(defun maybe-oomph-style ()
  "Use oomph's indentation style if needed"
  (when (is-oomph-code)
      (c-set-style "strict-oomph-c-style")
    (if (is-oomph-user-driver-code)
        (c-set-style "my-oomph-c-style"))))

(add-hook 'c-mode-common-hook 'maybe-oomph-style)


;; oomph-lib safe auto-whitespace cleanup
;; ============================================================
(defun whitespace-cleanup-if-not-oomph ()
  (interactive)
  (when (and (not (is-oomph-code))
             (not (string= major-mode "makefile-mode")))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'whitespace-cleanup-if-not-oomph)


;; Batch formatting function to oomph-lib style
;; ============================================================
(defun clean-whitespace-oomph-lib ()
   "Format the whole buffer."
   (interactive)

   ;; emacs doesn't think .h files are c++ by default so tell it to use c++
   ;; mode.
   (c++-mode)

   ;; Set the style depending on the file location
   (maybe-oomph-style)
   (message "Using style " c-indentation-style)

   ;; Clean up:
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace)
   (save-buffer))


;; Highlights for newton solve output
;; ============================================================
(defun oomph-highlight-trace ()
  "Highlight important parts of oomph-lib output."
  (interactive)
  (highlight-regexp "Timestep [0-9]+" 'hi-green)
  (highlight-regexp "Newton Step [0-9]+" 'hi-yellow)
  (highlight-regexp "Number of iterations to convergence: [0-9]+" 'hi-blue))



;; Create tags files
;;=================================================================
(setq path-to-ctags "/usr/bin/ctags")
;; source directorys to use go here:
;;(setq tags-source-directories "src/generic src/meshes user_drivers/micromagnetics")
(setq tags-source-directories "src/")
(defun create-oomph-tags ()
  (interactive)
  "Create tags file."
  (shell-command
   (format "cd ~/oomph-lib/; %s -e --extra=+q --recurse %s" path-to-ctags tags-source-directories))
  )



;; ;; Create tags files (all of them - for messing with const functions)
;; ;; Work in oomph-lib-2 directiory to be safer
;; ;;=================================================================
;; (setq path-to-ctags "/usr/bin/ctags")
;; ;; source directorys to use go here:
;; (setq tags-source-directories "demo_drivers/ src/")
;; (defun create-oomph-tags ()
;;   (interactive)
;;   "Create tags file."
;;   (shell-command
;;    (format "cd ~/oomph-lib-2/trunk/; %s -e --links=no --recurse --exclude=\"Makefile.*\" %s" path-to-ctags tags-source-directories))
;;   )

;; Search and replaces for converting MATLAB output to c++
;;================================================================
;; Narrow buffer to selection to make sure we don't run over
;; entire buffer.
;; Have to go to beginning before each new replace.
;; Watch out for x y z in variable names
;; Got bored of changing from the simpler version of serach replace to
;; the recommended way to do it half way through.
;; \\& means subs the matched string
(defun MATLABtoCppsourcefn ()
  (interactive)
  "Search and replace to make MATLAB output usable"
  (narrow-to-region (mark) (point))
  (beginning-of-buffer)
  (replace-string "source[0]" "source")
  (beginning-of-buffer)
  (while (re-search-forward "3.1415[0-9]*" nil t)
    (replace-match "Pi" nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "[^a-zA-z]x" nil t)
    (replace-match "\\&[0]" nil nil))
  (beginning-of-buffer)
  (replace-regexp "y" "x[1]")
  (beginning-of-buffer)
  (replace-regexp "z" "x[2]")
  (beginning-of-buffer)
  (replace-string "llg_damping_coeff" "llg_damping_coeff(t,x)")
  (beginning-of-buffer)
  (replace-string "llg_precession_coeff" "llg_precession_coeff(t,x)")
  (beginning-of-buffer)
  (replace-string "exchange_coeff" "exchange_coeff(t,x)")
  (widen))

(defun oomph-insert-include-guard ()
  (interactive)
  (if (buffer-file-name)
      (let* (
             ;; Get filename
             (fName (upcase (file-name-nondirectory
                             (file-name-sans-extension buffer-file-name))))
             (guard-name (concat "OOMPH_" fName "_H")))

        ;; Save current positions to come back to later
        (save-excursion
          (goto-char (point-min)) (insert "#ifndef " guard-name
                                          "\n#define " guard-name "\n")
          (goto-char (point-max)) (insert "\n#endif\n")))
    ;;else
    (message "Buffer must have a filename"))
  )

(defun oomph-insert-headers ()
  (interactive)
  (goto-char (point-min))
  (insert "
/*
description of file goes here
*/

#include \"generic.h\"

using namespace oomph;

namespace oomph
{
")
  (goto-char (point-max))
  (insert "
} // End of oomph namespace
")
  (oomph-insert-include-guard)
  (forward-line -3))

(defun oomph-open-files ()
  (interactive)
  (setq oomph-mm-base "~/oomph-lib/user_drivers/micromagnetics/")
  (setq oomph-working-folder "preconditioner_test/")

  ;; open general micromagnetics files
  (find-file (format "%s/*micromagnetics*" oomph-mm-base) "*") ;; "*" means * is a wildcard
  (find-file (format "%s/magnetic_materials.h" oomph-mm-base))
  (find-file (format "%s/my_general_header.h" oomph-mm-base))
  (find-file (format "%s/vector_helpers.h" oomph-mm-base))
  (find-file (format "%s/boundary_element_handler.h" oomph-mm-base))
  (find-file (format "%s/generic_poisson_problem.h" oomph-mm-base))

  ;; TAGS
  (visit-tags-table (format "%s/../../TAGS" oomph-mm-base))

  ;; ;; open all c++, shell and gnuplot files in "oomph-working-folder"
  ;; (find-file (format "%s/%s/*.sh" oomph-mm-base oomph-working-folder) "*")
  ;; (find-file (format "%s/%s/*.gp" oomph-mm-base oomph-working-folder) "*")
  ;; ;; driver file last so that it opens on top
  ;; (find-file (format "%s/%s/*.cc" oomph-mm-base oomph-working-folder) "*")
)



(defun sprintf-to-ofstream (file)
  (interactive "sofstream object name: ")
  (back-to-indentation)
  (let* ((start (point))
         arglist
         sprintf-args
         ofstream-args
         (end
          (save-excursion
            (search-forward ";")
            (point)))
         (sprintf-statement (buffer-substring-no-properties start end)))
    (unless (string= (substring sprintf-statement 0 7) "sprintf")
      (error "No `sprintf' at this line"))
    (setq arglist
          (split-string
           (substring sprintf-statement (1+ (position ?\( sprintf-statement))
                      (position ?\) sprintf-statement :from-end t))
           "\\s-*,\\s-*")
          sprintf-args
          (split-string (substring (cadr arglist) 1 -1)
                        "%[^[:alpha:]%#]*[[:alpha:]%#]")
          ofstream-args
          (with-output-to-string
            (princ (concat "\"" (car sprintf-args) "\""))
            (setq sprintf-args (cdr sprintf-args))
            (dotimes (i (length sprintf-args))
              (princ " + ")
              (when (< (+ i 2) (length arglist))
                (princ (nth (+ i 2) arglist))
                (princ " + "))
              (princ (concat "\"" (nth i sprintf-args) "\"")))))
    (kill-region start end)
    (insert (concat "ofstream " file "((" ofstream-args ").c_str());" ))))


(defun kill-line-including-newline ()
  (interactive)
  (delete-region (move-beginning-of-line 1)
                 (save-excursion
                   (forward-line 1)
                   (point))))

(defun replace-sprintf-ofstream (new-ofstream)
  (interactive "snew ofstream object name: ")

  ;; Go to next instance of sprintf
  (search-forward "sprintf(")

  ;; Find the region we are interested in
  (let* ((start (save-excursion (beginning-of-line) (point)))

         ;; ;; Find the declaration of ofstream and get its name
         ;; (old-ofstream
         ;;  (buffer-substring-no-properties
         ;;   (progn (search-backward "ofstream ") (match-end 0))
         ;;   (progn (search-forward ";") (match-beginning 0))))

         (old-ofstream "some_file")

         (end (save-excursion (search-forward (concat old-ofstream ".close()"))))
         )

    (print start)
    (print end)

    ;; Convert sprintf using function from ...
    (goto-char start)
    (sprintf-to-ofstream new-ofstream)

    ;; Kill some_file.open() statement
    (search-forward (concat old-ofstream ".open("))
    (kill-line-including-newline)

    ;; (goto-char start)
    (print new-ofstream)
    (print old-ofstream)

    ;; Replace ofstream name in any output functions and the .close()
    ;; statement
    (replace-string old-ofstream new-ofstream nil
                    start end)

    ;; strip .c_str() from doc_info.directory()
    (replace-string "doc_info.directory().c_str()" "doc_info.directory()"
                    nil start end)

    ;; add to_string to doc_info.number()
    (replace-string "doc_info.number()" "to_string(doc_info.number())"
                    nil start end)

    ;; variables called label are normally numbers so convert them
    (replace-string "label" "to_string(label)"
                    nil start end)

    ;; done?
    ))

(defun clean-up-after-sprintf-replace ()
  (interactive)
  (let* ((start (save-excursion (search-backward "{")))
         (end (save-excursion (search-forward "}"))))

    ;; Kill old ofstream and filename declarations
    (goto-char start)
    (while (search-forward "ofstream some_file;" end t)
      (kill-line-including-newline))

    (goto-char start)
    (while (search-forward "char filename[100];" end t)
      (kill-line-including-newline))

    (goto-char start)
    (while (search-forward "\"\" +" end t)
      (replace-match ""))

    ;; insert namespace command
    (goto-char start)
    (end-of-line) (newline) (newline)
    (insert-string "using namespace StringConversion;")

    ;; reindent
    (indent-region start end)
    ))

(defun sprintf-remove-soln-exact-error ()
  (interactive)
  (replace-sprintf-ofstream "soln_file")
  (replace-sprintf-ofstream "exact_file")
  (replace-sprintf-ofstream "error_file")
  (clean-up-after-sprintf-replace)
  (save-buffer))

(defun sprintf-remove-soln ()
  (interactive)
  (replace-sprintf-ofstream "soln_file")
  (clean-up-after-sprintf-replace)
  (save-buffer))

(defun convert-to-scipy ()
  (interactive)
  (replace-regexp "^" "[" '() (point-min) (point-max))
  (replace-regexp "$" "]," '() (point-min) (point-max))
  (replace-regexp "[ ]+" "," '() (point-min) (point-max)))
