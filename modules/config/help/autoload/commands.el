;;; commands.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +jg-help-list-package-locs (pkg)
  (interactive "sPacakge Name: \n")
  (message "Getting Locs of: %s" pkg)
  (let ((buff (get-buffer-create "*dpkg*")))
    (with-current-buffer buff (erase-buffer))
    (apply #'call-process "dpkg" nil buff nil "-L" pkg nil)
    (display-buffer buff)
    )
  )



;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    October 15, 2025
;; Modified:   October 15, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; commands.el ends here
