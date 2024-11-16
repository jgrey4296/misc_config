;;; util.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun +jg-workspaces-rebuild-persp-cache ()
  (interactive)
  (setq persp-names-cache
        (cl-loop for name in (hash-table-keys *persp-hash*)
                 if (not (string-equal name persp-nil-name))
                 collect name
                 )
        )

  (cl-loop for key being the hash-keys of *persp-hash*
           using (hash-values persp)
           if (or (string-equal key persp-nil-name)
                  (null persp))
           do
           (message "Found a nil perspective: %s" key)
           (remhash key *persp-hash*)
           )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 16, 2024
;; Modified:   November 16, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; util.el ends here
