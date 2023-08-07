;;; pytest.el -*- lexical-binding: t; -*-
(require 'transient)

;;;###autoload (autoload '+jg-python-test-logfile "lang-weakly-typed/python/autoload/pytest.el" nil t)
(transient-define-argument +jg-python-test-logfile ()
  " add logging to file "
  :class 'transient-option
  :description "Logfile"
  :argument "--log-file="
  :allow-empty t
  :reader #'(lambda (prompt &optional initial-input history )
              (format "%s.log" (f-join (f-parent (buffer-file-name)) (f-base (buffer-file-name)))))
  )

(transient-define-prefix +jg-python-pytest-dispatch ()
  "Show popup for running pytest."
  :incompatible '(("--exitfirst" "--maxfail="))
  :value '("--color")
  ["Output"
   [("-c" "color" "--color")
    ("-q" "quiet" "--quiet")
    ("-s" "no output capture" "--capture=no")
    (python-pytest:-v)]]
  ["Selection, filtering, ordering"
   [(python-pytest:-k)
    (python-pytest:-m)
    "                                          "] ;; visual alignment
   [("--dm" "run doctests" "--doctest-modules")
    ("--nf" "new first" "--new-first")
    ("--sw" "stepwise" "--stepwise")
    ("--co" "collect only" "--collect-only")]]
  ["Failures, errors, debugging"
   [("-l" "show locals" "--showlocals")
    ("-p" "debug on error" "--pdb")
    ("-x" "exit after first failure" "--exitfirst")]
   [("--ff" "failed first" "--failed-first")
    ("--ft" "full tracebacks" "--full-trace")
    ("--mf" "exit after N failures or errors" "--maxfail=")
    ("--rx" "run xfail tests" "--runxfail")
    (python-pytest:--tb)
    ("--tr" "debug on each test" "--trace")]]
  ["Options for pytest-xdist"
   [(python-pytest:-n)]
   [("-f" "loop on failure" "--looponfail")]]
  ["Run tests"
   [("t" "all" python-pytest)]
   [("r" "repeat" python-pytest-repeat)
    ("x" "last failed" python-pytest-last-failed)]
   [("f" "file (dwim)" python-pytest-file-dwim)
    ("F" "file (this)" python-pytest-file)]
   [("m" "files" python-pytest-files)
    ("M" "directories" python-pytest-directories)]
   [("d" "def/class (dwim)" python-pytest-function-dwim)
    ("D" "def/class (this)" python-pytest-function)]])
