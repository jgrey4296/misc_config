;;; compile-commands.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-workspaces-get-doot-commands (&optional dir)
  " get tasks from doot, cache them, then wrap them for use in ivy "
  ;; add to counsel-compile-local-builds
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (doot-toml (f-join root "doot.toml"))
               (dt-exists (f-exists? doot-toml))
               )
    (+jg-projects-doot-tasks)
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
