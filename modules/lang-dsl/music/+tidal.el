;;; +tidal.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! tidal
  :commands (tidal-mode tidal-start-haskell)
  :config
  (setq-default
   tidal-interpreter "/usr/bin/ghci"
   ;; tidal-interpreter-arguments (list "-ghci-script" (expand-file-name "~/github/languageLearning/tidal/.ghci"))
   )


  )

(speckler-add! librarian-regular ()
  (tidal-mode
   ("Tidal manual" . "https://tidalcycles.org/docs/")
   )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    October 15, 2025
;; Modified:   October 15, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +tidal.el ends here
