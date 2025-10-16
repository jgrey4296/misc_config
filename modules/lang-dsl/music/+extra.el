;;; +extra.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! faustine
  :commands faustine-mode
  :config
  ;; HACK Both `faust-mode' and `faustine-mode' are hardcoded to use
  ;; auto-complete. This silences the obnoxious 'You really should install and
  ;; use auto-complete' warnings when starting them.
  (defvar ac-modes nil)
  (defvar ac-sources nil)

  (map! :localleader
        :map faustine-mode-map
        "RET" #'faustine-mdoc
        "b" #'faustine-build
        "B" #'faustine-build-all
        "c" #'faustine-syntax-check
        "d" #'faustine-diagram
        "D" #'faustine-diagram-all
        "h" #'faustine-online-doc
        "o" #'faustine-toggle-output-buffer
        "s" #'faustine-source-code
        "r" #'faustine-run)
  )

(speckler-add! auto-modes ()
  '(faust
    ("\\.dsp\\'" . faustine-mode)
    )
  )

(speckler-add! company ()
  '((faust-mode faustine-mode)
    (:mode . #'+faust-company-backend))
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
;;; +extra.el ends here
