;;; describe.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-help-describe-env-var ()
  (interactive)
  (let* ((var (read-envvar-name "test"))
         (val (getenv var))
         )
    (with-temp-buffer-window "*helpful Environment Variable*" #'pop-to-buffer nil
      (princ (format "Environment Variable: %s\n\n" var))
      (princ (format "Value:\n\n%s" val))
      )
    )
)


;;-- Footer
;; Copyright (C) 2024 john grey
;;
;; Author:     john grey <https://github.com/jgrey4296>
;; Maintainer: john grey <jgrey4296@gmail.com>
;; Created:    June 10, 2024
;; Modified:   June 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; describe.el ends here
