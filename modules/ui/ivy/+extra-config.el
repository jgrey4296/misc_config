;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t)
  )

(use-package! flx
  :when (modulep! +fuzzy)
  :after ivy
  :preface
  (when (or (not (modulep! +fuzzy))
            (modulep! +prescient))
    (setq ivy--flx-featurep nil))
  :init
  (setq ivy-flx-limit 10000)
  )

(use-package! amx
  :config
  ; used by `counsel-M-x'
  (setq amx-save-file (concat doom-cache-dir "amx-items"))
  )

(use-package! general-insert :defer t)

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
