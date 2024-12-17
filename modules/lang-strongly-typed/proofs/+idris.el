;;; +idris.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! idris-mode
  :commands idris-mode
  :config
  (add-hook 'idris-mode-hook #'turn-on-idris-simple-indent)
  )

(speckler-add! librarian-regular
                    '(idris-mode
                      ("Idris Documentation" . "https://www.idris-lang.org/pages/documentation.html")
                      ("Idris Manual" . "https://idris2.readthedocs.io/en/latest/index.html")
                      )
                    )

(speckler-add! repl
                    '(idris-mode :start idris-pop-to-repl)
                    )

(speckler-add! lookup-handler
                    `(idris-mode
                      :documentation #'idris-docs-at-point
                      )
                    )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 09, 2024
;; Modified:   September 09, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +idris.el ends here
