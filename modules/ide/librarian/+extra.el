;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! company-gtags
  :when (modulep! :ide company)
  )

(use-package! company-ispell
  :when (modulep! :ide company)
)

(use-package! company-keywords
  :when (modulep! :ide company)
)

(use-package! ivy-xref
  :when (modulep! :ui ivy)
  :config
  (advice-add 'ivy-xref-show-xrefs :around #'+lookup--fix-ivy-xrefs)
  )

(use-package! dash-docs
  :defer t
  :config
  (setq dash-docs-enable-debugging init-file-debug
        dash-docs-docsets-path (expand-file-name "~/_cache_/docsets")
        dash-docs-min-length 2
        dash-docs-browser-func #'browse-url
        )

  (require 'counsel-dash nil t)
)

(use-package! wordnut
  :defer t
  :init
  (add-hook 'wordnut-mode-hook 'outline-minor-mode)
  )

(use-package! osx-dictionary
              :when (eq system-type 'darwin)
              :defer t)

(use-package! synosaurus :defer t)

(use-package! helm-wordnet
  :when (modulep! :ui helm)
  :defer t
  )

(use-package! define-word :defer t)

(use-package! browse-url :defer t)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
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
