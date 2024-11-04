;;; compile-commands.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-rst-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (project (f-join root "conf.py"))
               (project-exists (f-exists? project))
               )
    ;; Sphinx
    (+jg-projects-pair-cmds
     '("sphinx docs" "doot docs::build")
     (when (and (buffer-file-name) (f-ext? (buffer-file-name) "rst"))
       `("sphinx docfile" ,(format "doot docs::build.file %s" (buffer-file-name))))
     )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 04, 2024
;; Modified:   November 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; compile-commands.el ends here
