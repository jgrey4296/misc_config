;;; +origami.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! origami :defer t)

(speckler-add! fold ()
  `(origami
    :modes (origami-mode)
    :priority -25
    :triggers (:open-all   #'+jg-origami-open-all-nodes
               :close-all  #'+jg-origami-close-all-nodes
               :toggle     #'+jg-origami-toggle-node
               :open       #'+jg-origami-open-node
               :open-rec   #'+jg-origami-open-node-recursively
               :close      #'+jg-origami-close-node
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
;;; +origami.el ends here
