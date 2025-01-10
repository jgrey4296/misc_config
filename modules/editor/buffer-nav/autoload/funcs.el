;;; funcs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defvar jg-buffer-nav--segment-name-pattern "*RO:%s:<%s:%s>*")

;;;###autoload
(defun +jg-buffer-nav-make-read-only-segment ()
  "Create a read-only buffer of the selected region"
  (interactive)
  (unless (eq evil-state 'visual)
    (user-error "Select a segment"))

  (let* (text
         (seg-buff-name (format jg-buffer-nav--segment-name-pattern
                                (buffer-name)
                                (line-number-at-pos evil-visual-beginning)
                                (line-number-at-pos evil-visual-end)
                                ))
         )
    (with-current-buffer (current-buffer)
      (setq text (buffer-substring evil-visual-beginning
                                   evil-visual-end))
      )
    (with-current-buffer (get-buffer-create seg-buff-name)
      (insert text)
      (put-text-property (point-min)
                         (point-max)
                         'read-only t
                         )
      (set-buffer-modified-p nil)
      )
    (switch-to-buffer-other-window seg-buff-name)
    )
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 10, 2025
;; Modified:   January 10, 2025
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
;;; funcs.el ends here
