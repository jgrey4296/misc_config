;;; +outline.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
;; https://github.com/emacs-tree-sitter/treesit-fold
;built-in

(use-package! treesit-fold
  :after treesit
  )

(speckler-add! fold ()
  `(treesit
    :modes (treesit-fold-mode)
    :priority -25
    :triggers (:open-all   #'treesit-fold-open-all
               :close-all  #'treesit-fold-close-all
               :toggle     #'treesit-fold-toggle
               :open       #'treesit-fold-open
               :open-rec   #'treesit-fold-open-recursively
               :close      #'treesit-fold-close
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
