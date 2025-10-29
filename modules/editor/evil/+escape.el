;;; +escape.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! evil-escape-hook
  :hook (doom-first-input . evil-escape-mode)
  :config
  (evil-escape-add-default-inhibitors)
  )

;; --------------------------------------------------

(setq evil-escape-key-sequence "jk")

(speckler-setq! evil-escape ()
  evil-escape-state-blacklist      '(multiedit emacs motion)
  evil-escape-major-mode-blacklist '(neotree-mode treemacs-mode vterm-mode ibuffer-mode image-mode)
  evil-escape-key-sequence "jk"
  evil-escape-delay 0.15
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 27, 2025
;; Modified:   July 27, 2025
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
;;; +escape.el ends here
