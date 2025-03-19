;;; +spec-defs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;; TODO handle copy of other entries
(speckler-new! ivy-actions (key vals)
  "Delayed registration of ivy actions"
  :override nil
  :struct '(id-sym actiontriple*)
  :loop 'do
  (ivy-add-actions key
                   (mapcar (lambda (x)
                             (cl-destructuring-bind (key fn name) x
                                 (list key (upfun! fn) name))
                             )
                           vals)
                   )
  )



;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 24, 2024
;; Modified:   December 24, 2024
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
;;; +spec-defs.el ends here
