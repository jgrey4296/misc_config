;;; scratch.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


;;;###autoload
(defun +jg-window-nav--system-scratch ()
  "Open the system wide scratch buffer"
  (interactive)
  (let ((buffname (format "*scratch::system*"))
        )
    (display-buffer (get-buffer-create buffname))
    )
  )

;;;###autoload
(defun +jg-window-nav--project-scratch ()
  "open the project wide scratch buffer"
  (interactive)
  (let ((buffname (format "*scratch::%s*" (or (projectile-project-name) "anon")))
        (dir (projectile-acquire-root))
        )
    (display-buffer (get-buffer-create buffname))
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 16, 2024
;; Modified:   April 16, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; scratch.el ends here
