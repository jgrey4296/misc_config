;;; +outline.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


;built-in
(use-package! outline)


(setq outline-blank-line nil)

(speckler-add! fold ()
  `(outline
    :modes (outline-mode outline-minor-mode markdown-mode)
    :priority -25
    :triggers (:open-all   #'outline-show-all
               :close-all  ,(cmd! (with-no-warnings (outline-hide-sublevels 1)))
               :toggle     #'outline-toggle-children
               :open       ,(cmd! (with-no-warnings (outline-show-entry) (outline-show-children)))
               :open-rec   #'outline-show-subtree
               :close      #'outline-hide-subtree
               )
    )
  )
;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 09, 2025
;; Modified:   February 09, 2025
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
;;; +outline.el ends here
