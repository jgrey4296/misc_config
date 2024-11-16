;;; evil-motions.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


;;;###autoload (autoload '+jg-term-column-motion "tools/term/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-term-column-motion (count)
  "Augment evil-goto-column to go to end of line if at beginning"
  (cond (count
         (move-to-column 0)
         (comint-next-prompt 1)
         (forward-char count))
        ((< (point) (line-end-position))
         (end-of-line))
        (t
         (move-to-column 0)
         (comint-next-prompt 1))
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
;;; evil-motions.el ends here
