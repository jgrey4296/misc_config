;;; +tags.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(require 'cl-lib)
(require 'librarian-tagging)

(cl-defmethod librarian-set-tags ((mode (eql 'org-mode)) add sub keep)
  (let ((joined(append add keep))
         )
    (org-set-tags joined)
    )
  )

(cl-defmethod librarian-set-new-tags ((mode (eql 'org-mode)) new)
  "Utility to set a new tag for an org heading"
  (org-set-tags new)
  )

(cl-defmethod librarian-get-tags ((mode (eql 'org-mode)))
  (org-get-tags nil t)
  )

(cl-defmethod librarian-get-buffer-tags ((mode (eql 'org-mode)))
  (org-get-buffer-tags)
  )

(cl-defmethod librarian-backward-entry ((mode (eql 'org-mode)))
  (evil-backward-section-begin)
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 31, 2024
;; Modified:   May 31, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +tags.el ends here
