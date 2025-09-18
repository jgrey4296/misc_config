;;; +logic.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


(use-package! instal-mode      :defer t)

(use-package! sweeprolog       :defer t)

(use-package! pasp-mode
  :commands pasp-mode
  :after evil
  :init
  (setq-hook! 'pasp-mode-hook
    indent-line-function '+jg-logic-pasp-indent
    )
  (advice-add 'pasp-generate-command   :around #'+jg-pasp-generate-args)
  (advice-add 'pasp-run-clingo         :override #'+jg-pasp-run-clingo)
  (advice-add 'pasp-compilation-filter :override #'+jg-pasp-compilation-filter)
  )

(use-package! prolog
  :init
  (add-hook! 'prolog-mode-hook
             #'librarian-insert-minor-mode
             #'outline-minor-mode)

  (setq-hook! 'prolog-mode-hook
    outline-regexp (rx (+? nonl) ":-")
    outline-heading-end-regexp (rx line-end)
    outline-level #'(lambda () 1)
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 15, 2025
;; Modified:   September 15, 2025
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
;;; +logic.el ends here
