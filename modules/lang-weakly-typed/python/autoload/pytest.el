;;; pytest.el -*- lexical-binding: t; -*-
(require 'transient)
(require 'python-pytest)

(transient-define-argument +jg-python-test-logfile ()
  " add logging to file "
  :class 'transient-option
  :description "Logfile"
  :argument "--log-file="
  :allow-empty t
  :reader #'(lambda (prompt &optional initial-input history )
              (format "%s.log" (f-join (f-parent (buffer-file-name)) (f-base (buffer-file-name)))))
  )

(transient-define-prefix python-pytest-dispatch ()
  "Show popup for running pytest."
  :incompatible '(("--exitfirst" "--maxfail="))
  :value '("--color" "--exitfirst")
  ["Output"
   [
     ("-r" "Report" "-r fEsxXp")
     ("-f" "Logfile" +jg-python-test-logfile)
     ("-c" "color" "--color")
     ("-s" "no output capture" "--capture=no")
     ("-q" "quiet" "--quiet")
     (python-pytest:-v)
     ]
   [ ("--tc" "Trace Config" "--trace-config")    ("--lc" "Log CLI" "--log-cli-level=INFO") ]
   ]
  ["Selection, filtering, ordering"
   [(python-pytest:-k) (python-pytest:-m)  ]
   [("--nf" "new first" "--new-first") ("--sw" "stepwise" "--stepwise") ("--co" "collect only" "--collect-only") ("--dm" "run doctests" "--doctest-modules") ]
   ]
  ["Failures, errors, debugging"
   [("-l" "show locals" "--showlocals")
    ("-d" "debug on error" "--pdb")
    ("-x" "exit after first failure" "--exitfirst" )
    ("-f" "failed first" "--failed-first")
    ]
   [
    ("--ft" "full tracebacks" "--full-trace")
    ("--mf" "exit after N failures or errors" "--maxfail=")
    ("--rx" "run xfail tests" "--runxfail")
    ("--f" "loop on failure" "--looponfail")
    (python-pytest:--tb)
    ("--tr" "debug on each test" "--trace")]]
  ["Options for pytest-xdist"
   [ (python-pytest:-n)   ]
   [  ]
   ]
  ["Run tests"
   [ ("RET" "All Project Tests"         python-pytest)
     ("DEL" "last failed"               python-pytest-last-failed)
     ("r"   "repeat"                    python-pytest-repeat)
     ]
   [
    ("f" "file (dwim)"                  python-pytest-file-dwim)
    ("d" "def/class (dwim)"             python-pytest-function-dwim)
    ("m" "files"                        python-pytest-files)
    ]
   [
    ("F" "file (this)"                  python-pytest-file)
    ("D" "def/class (this)"             python-pytest-function)
    ("M" "directories"                  python-pytest-directories)
    ]
   [
    ]
   ]
  transient-quit!
  )

;;;###autoload
(defun +jg-python-pytest-dispatch ()
  (interactive)
  (python-pytest-dispatch)
  )
