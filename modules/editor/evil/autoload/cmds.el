;;; cmds.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-evil-visual-indirect-buffer ()
  "Create a pre-narrowed indirect buffer
   In normal state: split and narrow current-buffer at point
   In visual state: narrow indirect to selection
"
  (interactive)
  (cond ((eq evil-state 'normal)
         (let ((split-point (line-beginning-position))
               (indirect (clone-indirect-buffer (format (buffer-name) "<split>") nil t))
               )
           (with-current-buffer (current-buffer) (narrow-to-region (point-min) split-point))
           (display-buffer indirect (cons #'display-buffer-below-selected '()))
           (with-current-buffer indirect (narrow-to-region split-point (point-max)))
           )
         )
        ((eq evil-state 'visual)
         (let ((region (cons (marker-position evil-visual-beginning) (marker-position evil-visual-end)))
               indirect
               )
           (evil-normal-state)
           (setq indirect (clone-indirect-buffer (format (buffer-name) "<region>") nil t))
           (display-buffer indirect (cons #'display-buffer-below-selected '()))
           (with-current-buffer indirect (narrow-to-region (car region) (cdr region)))
           )
         )
        )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 29, 2024
;; Modified:   April 29, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; cmds.el ends here
