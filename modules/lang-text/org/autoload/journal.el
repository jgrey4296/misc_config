;;; journal.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


;;;###autoload
(defun +org-journal-p ()
"Wrapper around `org-journal-is-journal' to lazy load `org-journal'."
(when-let (buffer-file-name (buffer-file-name (buffer-base-buffer)))
    (if (or (featurep 'org-journal)
            (and (file-in-directory-p
                buffer-file-name org-journal-dir)
                (require 'org-journal nil t)))
        (org-journal-is-journal))))


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    June 13, 2024
;; Modified:   June 13, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; journal.el ends here
