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

(transient-define-argument +jg-python-test-coverage-file ()
  "track test coverage"
  :class 'transient-option
  :description "Coverage"
  :argument "--cov="
  :allow-empty t
  :reader #'(lambda (&rest args) (buffer-file-name))
  )

(transient-define-argument +jg-python-test-coverage ()
  "track test coverage"
  :class 'transient-option
  :description "Coverage"
  :argument "--cov="
  :allow-empty t
  :reader #'(lambda (&rest args) (read-file-name "Track File: "))
  )

(transient-define-prefix +jg-python-pytest-dispatch ()
  "Show popup for running pytest."
  :man-page "pytest"
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
     (python-pytest:-W)
     ]
   [ ("--tc" "Trace Config" "--trace-config")
     ("--lc" "Log CLI"      "--log-cli-level=INFO")
     ("--cc" "Coverage"     +jg-python-test-coverage-file)
     ("--cv" "Coverage"     +jg-python-test-coverage)
     ]
   ]
  ["Selection, filtering, ordering"
   [(python-pytest:-k) (python-pytest:-m)  ]
   [("--nf" "new first" "--new-first") ("--sw" "stepwise" "--stepwise") ("--o" "collect only" "--collect-only") ("--dm" "run doctests" "--doctest-modules") ]
   ]
  ["Failures, errors, debugging"
   [("-l" "show locals" "--showlocals")
    ("-d" "debug on error" "--pdb")
    ("-f" "failed first" "--failed-first")
    ("-x" "exit after first failure" "--exitfirst" )
    ]
   [
    ("--mf" "exit after N failures or errors" "--maxfail=")
    ("--tf" "full tracebacks" "--full-trace")
    ("--rx" "run xfail tests" "--runxfail")
    (python-pytest:--tb)
    ("--tr" "debug on each test" "--trace")

    ]]
  ["Options for pytest-xdist"
   [ (python-pytest:-n)
     ("--f" "loop on failure" "--looponfail")
     ]
   [  ]
   ]
  ["Run tests"
   [ ("RET" "All Project Tests"         python-pytest)
     ("DEL" "last failed"               python-pytest-last-failed)
     ("r"   "repeat"                    python-pytest-repeat)
     ]
   [
    ("f" "file (dwim)"                  python-pytest-file-dwim)
    ("d" "def/class (dwim)"             python-pytest-run-def-or-class-at-point-dwim)
    ("m" "files"                        python-pytest-files)
    ]
   [
    ("F" "file (this)"                  python-pytest-file)
    ;; ("D" "def/class (this)"             python-pytest-function)
    ("M" "directories"                  python-pytest-directories)
    ]
   [
    ]
   ]
  transient-quit!
  )

;;;###autoload
(defun +jg-python-pytest ()
  (interactive)
  (+jg-python-pytest-dispatch)
  )

(defun +jg-python-test-extra-args (args)
  (plist-put args :args (transient-args '+jg-python-pytest-dispatch))
  )

(advice-add 'python-pytest--run :filter-args #'+jg-python-test-extra-args)
