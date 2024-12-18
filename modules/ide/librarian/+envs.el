;;; +envs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(speckler-new! lib-env (key val)
               "Registers environment entry/exit handlers. eg: for python"
               :struct '(or librarian-env-handler plist)
               :setup (librarian-envs-clear!)
               :loop 'do
               (if (librarian-envs-handler-p (car-safe val))
                   (librarian-envs-register! (car val))
                 (apply #'librarian-envs-register! :id key val))
               )


(map! :leader
      (:prefix ("c v" . "Environments")
      :desc "Activate Env" "a" #'librarian-envs-start!
      :desc "Clear Env"    "d" #'librarian-envs-stop!
      :desc "Report Env"   "r" #'librarian-envs-report!
      :desc "Lock Env"     "l" #'librarian-envs-toggle-lock!
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
