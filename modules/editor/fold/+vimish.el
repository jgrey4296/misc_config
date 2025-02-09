;;; +vimish.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! vimish-fold)

(setq-default vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
              vimish-fold-indication-mode 'right-fringe
              vimish-fold-header-width 50
              vimish-fold-persist-on-saving nil
              )

(speckler-add! fold ()
  :override nil
  `(vimish
    :modes (vimish-fold-mode)
    :priority -50
    :triggers (:delete     #'vimish-fold-delete
               :open-all   #'vimish-fold-unfold-all
               :close-all  #'vimish-fold-refold-all
               :toggle     #'vimish-fold-toggle
               :open       #'vimish-fold-unfold
               :open-rec   nil
               :close      #'vimish-fold-refold
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
;;; +vimish.el ends here
