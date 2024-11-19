;;; funcs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-eval-clean(arg)
  (interactive "P")
  (-when-let* ((root (projectile-project-root))
               (doot-toml (f-join root "doot.toml"))
               (dt-exists (f-exists? doot-toml))
               (default-directory root)
               )
    (let ((targets (+jg-projects-doot-tasks nil (lambda (x) (concat jg-projects-doot-cmd " clean " (when arg "-c ") (car (split-string x" " t " "))))))
          (counsel-compile--current-build-dir (or (counsel--compile-root) default-directory))
          )
      (ivy-read "Clean Task: " ivy-opts
                :action #'counsel-compile--action
                :caller 'jg-projects-clean)
      )
    )
  )





;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 15, 2024
;; Modified:   November 15, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; funcs.el ends here
