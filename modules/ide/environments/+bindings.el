;;; +bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(map! :leader
      (:prefix ("c v" . "Environments")
      :desc "Activate Env" "a" #'env-handling-go!
      :desc "Clear Env"    "d" #'env-handling-clear-env!
      :desc "Report Env"   "r" #'env-handling-report!
      :desc "Lock Env"     "l" #'env-handling-lock!
      :desc "Create venv"  "c" #'env-handling-create-env!
      )
)


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 05, 2024
;; Modified:   July 05, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +bindings.el ends here
