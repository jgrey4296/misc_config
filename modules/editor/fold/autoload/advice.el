;;; advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +fold--hideshow-ensure-mode-a (&rest _)
    "Ensure `hs-minor-mode' is enabled when we need it, no sooner or later."
    (unless (bound-and-true-p hs-minor-mode)
      (hs-minor-mode +1))
    )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 27, 2024
;; Modified:   May 27, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; advice.el ends here
