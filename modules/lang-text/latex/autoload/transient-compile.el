;;; transient-compile.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

(progn ;; define the calls
  ;; (transient-make-call! {name} "p" "Desc" (call) )
  ;; (transient-make-int-call! magit-todos         "t"   "Todos"             :transient nil #'magit-todos-list)
  ;; (transient-make-var-toggle! auto-balance evil-auto-balance-windows "Auto-Balance Windows" "B")
  ;; transient-define-infix
  ;; transient-define-argument


  ;; -draftmode
  ;; -no-file-line-error
  ;; -halt-on-error
  ;; -interaction= batchmode/nonstopmode/scrollmode/errorstopmode
  ;; -output-directory=
  ;; -no-parse-first-line
  ;; -no-shell-escape
  ;; -8bit

  ;; -lua={file}
  )

(transient-define-infix jg-test-infix ()
  "a test infix"
  :argument "--test"
  :shortarg "-s"
  :description "A short blah"
  )

(transient-define-argument jg-test-arg ()
  "test arg"
  :class 'transient-switches
  :unsavable t
  :argument-format "--blah=%s"
  :argument-regexp "\\(blah\\|bloo\\blee\\)"
  :choices '("blah" "bloo" "blee")
  )

(transient-define-suffix jg-test-call (args)
  "test call"
  :transient nil
  :description "a blah call"
  (interactive (list (transient-args (oref transient-current-prefix command))))
  (message "test call: %s" args)
  (transient-save)
  )

(transient-define-argument jg-latex-compiler ()
  "Animal picker."
  :argument "compiler="
  :allow-empty nil
  ; :multi-value t ; multi-value can be set to --animals=fox,otter,kitten etc
  :class 'transient-option
  :choices '("pdflatex" "lualatex" "xelatex")
  :init-value (lambda (obj) "pdflatex")
  )

;; Create the transient

;;;###autoload (autoload 'transient-latex-compile "lang-text/latex/autoload/transient-compile.el" nil t)
(transient-define-prefix transient-latex-compile ()
  ""
  ["Options"
   [
    (jg-test-infix)
    ("-e" "exclusive" jg-test-arg)
    ]
   [
    ("a" "an arg" "--arg=")
    ("b" "unsav arg" "--other=" :unsavable t)
    ("c" "compiler" jg-latex-compiler)
    ]
   ]
  ["Run"
   [("r" jg-test-call)]
   [("RET" jg-test-call)]
   ]
  transient-quit!
  )



;;-- Footer
;; Copyright (C) 2023 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 20, 2023
;; Modified:   November 20, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; transient-compile.el ends here
