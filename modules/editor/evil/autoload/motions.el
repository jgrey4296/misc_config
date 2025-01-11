;;; motions.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload (autoload '+jg-evil-find-char "editor/evil/autoload/motions.el" nil t)
(evil-define-motion +jg-evil-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil.
JG: Modified to respect case-fold-search
"
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and evil-respect-visual-line-mode
                     visual-line-mode))
        )
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (evil-forward-char 1 evil-cross-lines))
    (unless (prog1
                (search-forward
                 (char-to-string char)
                 (cond (evil-cross-lines nil)
                       ((and fwd visual)
                        (save-excursion
                          (end-of-visual-line)
                          (point)))
                       (fwd (line-end-position))
                       (visual
                        (save-excursion
                          (beginning-of-visual-line)
                          (point)))
                       (t (line-beginning-position)))
                 t count)
              (when fwd (backward-char)))
      (user-error "Can't find `%c'" char))))

;;;###autoload (autoload '+jg-evil-bob-with-mark "editor/evil/autoload/motions.el" nil t)
(evil-define-motion +jg-evil-bob-with-mark (count)
  "Jump to the first line, but pushing a mark as well"
  :jump t
  :type line
  (push-mark)
  (evil-goto-line (or count 1))
  )



;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 11, 2025
;; Modified:   January 11, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; motions.el ends here
