
(require 'validate)
(require 'flycheck)
(require 's)
(require 'f)
(require 'page-break-lines)
(require 'use-package)
(require 'projectile)

;; Automatically use python mode from "python-mode.el"
(require 'python)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; Override fill-function-arguments binding because I use black anyway and
;; python's autofill is janky.
(define-key python-mode-map (kbd "M-q") #'fill-paragraph)

(defun prettify-lambda ()
  (setq prettify-symbols-alist '(("lambda" . 955))))

(add-hook 'python-mode-hook #'prettify-symbols-mode)
(add-hook 'python-mode-hook #'prettify-lambda)
(add-hook 'python-mode-hook #'page-break-lines-mode)
(add-hook 'python-mode-hook #'flycheck-mode)

(use-package blacken
  :config
  (add-hook 'python-mode-hook #'blacken-mode))


(defun ds/python-setup-indent ()
  (setq-local indent-tabs-mode nil)
  (setq-local python-indent-offset 4))
(add-hook 'python-mode-hook #'ds/python-setup-indent)

;; Font lock for f-strings
(setq python-font-lock-keywords-maximum-decoration
      (append python-font-lock-keywords-maximum-decoration
              '(;; this is the full string.
                ;; group 1 is the quote type and a closing quote is matched
                ;; group 2 is the string part
                ("f\\(['\"]\\{1,3\\}\\)\\(.+?\\)\\1"
                 ;; these are the {keywords}
                 ("{\\([^}]*?\\)}"
                  ;; Pre-match form
                  (progn (goto-char (match-beginning 0)) (match-end 0))
                  ;; Post-match form
                  (goto-char (match-end 0))
                  ;; face for this match
                  (1 font-lock-variable-name-face t))))))


(defun ds/shell-to-python ()
  (interactive)
  (let ((shellified (-->
                     (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                     (s-trim it)
                     (s-split " " it)
                     (--map (s-replace "\"" "" it) it)
                     (-map (lambda (x)
                             (if (s-matches? "^\\$" x)
                                 (s-replace "$" "" x)
                               (s-concat "\"" x "\""))) it)
                     (s-join ", " it)
                     (s-concat "subprocess.check_call([" it "])"))))
    (delete-region (point-at-bol) (point-at-eol))
    (insert shellified)))


(validate-setq flycheck-flake8-maximum-line-length 120)
(validate-setq flycheck-python-pycompile-executable "pyenv-python3")

;; Shut up the warnining about my pycompile checker binary
(put 'flycheck-python-pycompile-executable 'safe-local-variable (lambda (value) (equal value  "/home/david/.pyenv/versions/3.7.4/bin/python3")))
(put 'flycheck-python-mypy-executable 'safe-local-variable (lambda (value) t))
(put 'flycheck-ds-python-dmypy-executable 'safe-local-variable (lambda (value) t))

(defun ds/flycheck-mypy-find-project-root (_checker)
  "Compute an appropriate working-directory for flycheck-mypy.
This is either a parent directory containing a flycheck-mypy.ini, or nil."
  (and
   buffer-file-name
   (locate-dominating-file buffer-file-name "mypy.ini")))

(flycheck-define-checker ds-python-dmypy
  "Mypy syntax and type checker daemon.

See URL `http://mypy-lang.org/'."
  :command ("dmypy"
            "run"
            "--"
            "src" "../wavelib" "unittests" "conftest.py"
            )
  :error-patterns
  ((error line-start (file-name) ":" line (optional ":" column)
          ": error:" (message) line-end)
   (warning line-start (file-name) ":" line (optional ":" column)
            ": warning:" (message) line-end)
   (info line-start (file-name) ":" line (optional ":" column)
         ": note:" (message) line-end))
  :modes python-mode
  ;; Ensure the file is saved, to work around
  ;; https://github.com/python/mypy/issues/4746.
  :predicate flycheck-buffer-saved-p
  :working-directory ds/flycheck-mypy-find-project-root)
(add-to-list 'flycheck-checkers 'ds-python-dmypy)



;; Magic imports
;; ============================================================

(defvar ds/python-stdlib
  '("abc" "anydbm" "argparse" "array" "asynchat" "asyncore" "atexit" "base64"
    "BaseHTTPServer" "bisect" "bz2" "calendar" "cgitb" "cmd" "codecs"
    "collections" "commands" "compileall" "ConfigParser" "contextlib" "Cookie"
    "copy" "cPickle" "cProfile" "cStringIO" "csv" "datetime" "dataclasses" "dbhash" "dbm"
    "decimal" "difflib" "dircache" "dis" "doctest" "dumbdbm" "EasyDialogs"
    "errno" "exceptions" "filecmp" "fileinput" "fnmatch" "fractions"
    "functools" "gc" "gdbm" "getopt" "getpass" "gettext" "glob" "grp" "gzip"
    "hashlib" "heapq" "hmac" "imaplib" "imp" "inspect" "itertools" "json"
    "linecache" "locale" "logging" "mailbox" "math" "mhlib" "mmap"
    "multiprocessing" "operator" "optparse" "os" "pdb" "pickle" "pipes"
    "pkgutil" "platform" "plistlib" "pprint" "profile" "pstats" "pwd" "pyclbr"
    "pydoc" "Queue" "random" "re" "readline" "resource" "reprlib" "rlcompleter"
    "robotparser" "sched" "select" "shelve" "shlex" "shutil" "signal"
    "SimpleXMLRPCServer" "site" "sitecustomize" "smtpd" "smtplib" "socket"
    "SocketServer" "sqlite3" "string" "StringIO" "struct" "subprocess" "sys"
    "sysconfig" "tabnanny" "tarfile" "tempfile" "textwrap" "threading" "time"
    "timeit" "trace" "traceback" "typing" "typing_extensions" "unittest" "urllib" "urllib2" "urlparse"
    "usercustomize" "uuid" "warnings" "weakref" "webbrowser" "whichdb" "xml"
    "xmlrpclib" "zipfile" "zipimport" "zlib" "builtins" "__builtin__"))

(defvar ds/python-third-party-libs
  '("requests" "pytest" "freezegun"))

;; TODO: extend existing import lines? Or just use isort for that?

(defun ds/python-importable-symbols (path)
  (when (f-exists-p path)
    (with-temp-buffer
      (shell-command (concat "sed -n "
                             "-e 's/^def \\([a-zA-Z][a-zA-Z0-9_]*\\).*/\\1/p' "
                             "-e 's/^class \\([a-zA-Z][a-zA-Z0-9_]*\\).*/\\1/p' "
                             "-e 's/^\\([a-zA-Z0-9_]*\\) *=.*/\\1/p' "
                             path) t "*python-importable-symbols-errors*")
      (--> (buffer-substring (point-min) (point-max))
           (s-split "\n" it)))))

(defun ds/import (insert-here)
  "Insert an import statement at the start of the file."
  (interactive "P")
  (let* ((files (--> (projectile-current-project-files)
                     (-filter (lambda (path) (or (s-ends-with-p ".py" path) (s-contains-p "_test" path))) it)
                     (append ds/python-stdlib ds/python-third-party-libs it)))
         (default-symbol (when (symbol-at-point) (symbol-name (symbol-at-point))))
         (file (completing-read "import file: " files))
         (file-symbols (ds/python-importable-symbols (f-join (projectile-project-root) file)))
         (individual-symbol (completing-read "symbol(s): " file-symbols nil nil default-symbol))
         (import-statement-line (ds/path-to-import-statement file individual-symbol)))
    (if insert-here
        (insert import-statement-line)
      (ds/insert-as-import import-statement-line))))

(defun ds/path-to-import-statement (path symbol)
  (let* ((module (--> path
                      (s-trim it)
                      (f-join (projectile-project-root) it)
                      (file-relative-name it (projectile-project-root))
                      (s-chop-prefix "money-srv/src/" it)
                      (s-chop-prefix "wavesms/src/" it)
                      (s-chop-prefix "wavemodem/src/" it)
                      (s-chop-prefix "wavelib/" it)
                      (s-chop-suffix ".py" it)
                      (s-replace "/" "." it)
                      )))
    (if (equal symbol "")
        (s-concat "import " module "\n")
      (s-concat "from " module " import " symbol "\n"))))

(defun ds/pick-import-location ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^def\\|^class" nil 'noerror)
    (re-search-backward "^import\\|^from" nil 'noerror)
    (beginning-of-line)
    (point)))

(defun ds/insert-as-import (line)
  (save-excursion
    (goto-char (ds/pick-import-location))
    (insert line)
    (message "Added import: %s" line)))

(define-key python-mode-map (kbd "C-,") #'ds/import)



;; Dicts to/from kwargs
;; ============================================================

(defun ds/kwargs-to-dict (start end)
  "Convert selected python kwargs to a dictionary"
  (interactive (list (region-beginning) (region-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert "{")
      (while (re-search-forward "\\([a-zA-Z_]*\\)=" nil t)
        (replace-match "\"\\1\": " nil nil))
      (goto-char (point-max))
      (insert "}"))))

(defun ds/dict-to-kwargs (start end)
  "Convert selected dictionary into kwargs (must select entire an dict)"
  (interactive (list (region-beginning) (region-end)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)

      (goto-char (point-min))
      (re-search-forward "{" nil)
      (replace-match "" nil nil)

      (goto-char (point-min))
      (while (re-search-forward "\"\\([a-zA-Z_0-9]*\\)\":" nil t)
        (replace-match "\\1=" nil nil))

      (goto-char (point-max))
      (re-search-backward "},?" nil)
      (replace-match "" nil nil))))
