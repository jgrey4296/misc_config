;;; +envs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(spec-handling-new! lib-env nil
                    :struct '(or librarian-env-handler plist)
                    :loop 'do
                    (if (librarian-envs-handler-p (car-safe val))
                        (librarian-envs-register (car val))
                      (apply #'librarian-envs-register :id key val))
                    )


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
;; Created:    December 14, 2024
;; Modified:   December 14, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +envs.el ends here
