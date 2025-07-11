;;; +flycheck.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! flycheck-cask
  :commands #'flycheck-cask-setup
  :init
  ;; (add-hook! 'emacs-lisp-mode-hook
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t))

  )

(use-package! flycheck-package
  :after flycheck
  :config
  (add-to-list 'flycheck-checkers 'emacs-lisp-package t)
  )

;; --------------------------------------------------

(speckler-setq! flycheck-lisp ()
  flycheck-emacs-lisp-load-path 'inherit
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 11, 2025
;; Modified:   July 11, 2025
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
;;; +flycheck.el ends here
