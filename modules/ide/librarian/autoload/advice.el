;;; advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +lookup--projectile-find-tag-a (fn)
  (let ((xref-backend-functions '(etags--xref-backend t)))
    (funcall fn))
  )

;;;###autoload
(defun +spell--fix-face-detection-a (fn &rest args)
    "`spell-fu--faces-at-point' uses face detection that won't penetrary
overlays (like `hl-line'). This makes `spell-fu-faces-exclude' demonstrably less
useful when it'll still spellcheck excluded faces on any line that `hl-line' is
displayed on, even momentarily."
    (letf!

      (defun get-char-property (pos prop &optional obj)
        (or (plist-get (text-properties-at pos) prop)
            (funcall get-char-property pos prop obj)))
      (apply fn args)))

;;;###autoload
(defun +spell--create-word-dict-a (_word words-file _action)
    "Prevent `spell-fu--word-add-or-remove' from throwing non-existant
directory errors when writing a personal dictionary file (by creating the
directory first)."
    (unless (file-exists-p words-file)
      (make-directory (file-name-directory words-file) t)
      (with-temp-file words-file
        (insert (format "personal_ws-1.1 %s 0\n" ispell-dictionary)))))

;;;###autoload
(defun +spell-init-ispell-extra-args-a (orig-fun &rest args)
  (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
    (ispell-kill-ispell t)
    (apply orig-fun args)
    (ispell-kill-ispell t))
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 14, 2024
;; Modified:   May 14, 2024
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
