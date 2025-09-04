;;; +outline-indent.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! outline-indent
  :config
  (setq outline-indent-ellipsis "|...")

  )

(speckler-add! fold ()
  `(outline
    :modes outline-indent-minor-mode
    :priority -25
    :triggers (:close     #'outline-indent-close-fold
               :close-all #'outline-indent-close-folds
               :open      #'outline-indent-open-fold
               :open-all  #'outline-indent-open-folds
               :open-rec  #'outline-indent-open-fold-rec
               :toggle    #'outline-indent-toggle-fold
               )
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    August 11, 2025
;; Modified:   August 11, 2025
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
;;; +outline-indent.el ends here
