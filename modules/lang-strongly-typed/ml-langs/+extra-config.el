;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! utop
    :after tuareg
    :hook (tuareg-mode-local-vars . +ocaml-init-utop-h)
    )

(use-package! flycheck-ocaml
  :after merlin
  :hook (merlin-mode . +ocaml-init-flycheck-h)
  )

(use-package! merlin-eldoc
  :after merlin
  :hook (merlin-mode . merlin-eldoc-setup)
  )

(use-package! merlin-iedit
  :after merlin
  )

(use-package! merlin-imenu
  :after merlin
  :hook (merlin-mode . merlin-use-merlin-imenu)
  )

(use-package! ocp-indent
  :after tuareg
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :hook (tuareg-mode-local-vars . +ocaml-init-ocp-indent-h)
  )

(use-package! ocamlformat
  :commands ocamlformat
  :hook (tuareg-mode-local-vars . +ocaml-init-ocamlformat-h)

  )

(use-package! company-mlton
  :when (modulep! :ide company)
  :after sml-mode
  :hook (sml-mode . company-mlton-init)
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
;;; +extra-config.el ends here
