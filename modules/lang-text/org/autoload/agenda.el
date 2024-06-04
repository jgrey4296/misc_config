;;; agenda.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-org-list-agenda-files ()
  " Creates a temporary, Org-mode buffer with links to agenda files "
  (interactive)
  (with-output-to-temp-buffer "*Agenda Files*"
    (set-buffer "*Agenda Files*")
    (insert "#title: Agenda Files ")
    (let ((core (sort (-select #'(lambda (x) (f-descendant-of? x org-directory)) org-agenda-files) #'string-lessp))
          (spec (sort (-remove #'(lambda (x) (f-descendant-of? x org-directory)) org-agenda-files) #'string-lessp))
          )
      (insert "\n\n* Core:\n")
      ;; Insert core agenda files:
      (cl-loop for file in core
               do
               (insert (format "** [[%s][%s]]\n" file (f-base file)))
               )
      (insert "\n* Specific:\n")
      ;; Insert Repo specific files
      (cl-loop for file in spec
               do
               (insert (format "** [[%s][%s]]\n" file (f-base file)))
               )
    )
    (org-mode)
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    June 04, 2024
;; Modified:   June 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; agenda.el ends here
