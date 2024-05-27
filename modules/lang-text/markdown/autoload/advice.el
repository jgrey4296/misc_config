;;; advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


;;;###autoload
(defun +markdown-disable-front-matter-fontification-a (&rest _)
  " HACK Prevent mis-fontification of YAML metadata blocks in `markdown-mode'
       which occurs when the first line contains a colon in it. See
       jrblevin/markdown-mode#328. "
    (ignore (goto-char (point-max))))

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
