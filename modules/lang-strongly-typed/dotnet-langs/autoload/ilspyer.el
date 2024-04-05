;;; ilspyer.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-dotnet-dired-ilspy ()
  " run ilspycmd dis on the file, or a function, according to visual state "
  (interactive)
  (cl-loop for file in (dired-get-marked-files)
           do
           (let ((name (format "%s-dis" (f-base file)))
                 (dis-file (format "%s.dis" file))
                 )
             (make-process :name name
                           :buffer (format "*%s*" name)
                           :command (list "ilspycmd" file)
                           :noquery t
                           :sentinel (-partial
                                      #'+jg-dotnet-ilspy--sentinel-writer
                                      dis-file)
                           )
             )
           )
  )


(defun +jg-dotnet-ilspy--sentinel-writer (target proc stat)
  (when (not (process-live-p proc))
    (with-current-buffer (process-buffer proc)
      (write-file target)
      )
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 05, 2024
;; Modified:   April 05, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; ilspyer.el ends here
