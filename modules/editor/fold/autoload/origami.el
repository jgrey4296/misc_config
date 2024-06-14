;;; origami.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-origami-toggle-node ()
  (interactive)
  (origami-toggle-node (current-buffer) (point))
  )

;;;###autoload
(defun +jg-origami-open-all-nodes ()
  (interactive)
  (origami-open-all-nodes (current-buffer))
  )

;;;###autoload
(defun +jg-origami-close-all-nodes ()
  (interactive)
  (origami-close-all-nodes (current-buffer))
  )

;;;###autoload
(defun +jg-origami-open-node ()
  (interactive)
  (origami-open-node (current-buffer) (point))
  )

;;;###autoload
(defun +jg-origami-close-node ()
  (interactive)
  (origami-close-node-recursively (current-buffer) (point))
  )

;;;###autoload
(defun +jg-origami-open-node-recursively ()
  (interactive)
  (origami-open-node-recursively (current-buffer) (point))
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 13, 2024
;; Modified:   May 13, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; origami.el ends here
