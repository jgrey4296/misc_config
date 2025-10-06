;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! cdlatex
  :disabled t
  :after auctex
  :config
  ;; Use \( ... \) instead of $ ... $.
  (setq cdlatex-use-dollar-to-ensure-math nil)
  )

(use-package! adaptive-wrap
  :commands adaptive-wrap-prefix-mode
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init
  (setq-default adaptive-wrap-extra-indent 0)
  ;; Nicely indent lines that have wrapped when visual line mode is activated.
  )

(use-package! evil-tex
  :disabled t
  :after auctex
  )

(use-package! company-auctex
  :when (modulep! :ide company)
  :after auctex

  )

(use-package! company-math
  :when (modulep! :ide company)
  :after auctex
  )

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
