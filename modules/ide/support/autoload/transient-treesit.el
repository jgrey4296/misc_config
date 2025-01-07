;;; transient-treesit.lsp -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(require 'macro-tools--transient)

(transient-call! treesit-inspect ()
  "Turn on treesit syntax tree inspection"
  :key "t"
  :desc "Inspect Treesit AST"
  :interactive t
  #'treesit-inspect-mode
  )
(transient-call! treesit-increase-font ()
  "Increase the fontification level"
  :key "+"
  :desc "Increase Font Level"
  (setq-default treesit-font-lock-level (min 4 (1+ treesit-font-lock-level)))
  (treesit-font-lock-recompute-features)
  )
(transient-call! treesit-decrease-font ()
  "Decrease the fontification level"
  :key "-"
  :desc "Decrease Font Level"
  (setq-default treesit-font-lock-level (max 0 (1- treesit-font-lock-level)))
  (treesit-font-lock-recompute-features)
  )
(transient-call! treesit-set-font ()
  "Increase the fontification level"
  :key "="
  :desc (format "Set Font Level (%s)" treesit-font-lock-level)
  :interactive t
  #'treesit-change-fontification-level
  )

;;;###autoload
(defun +jg-support-build-treesit-transient ()
  (transient-append-suffix (cadr jg-toggle-debugs-transient)
    "f" '(transient-macro-call-treesit-inspect)
    )
  (transient-append-suffix 'jg-toggle-main
    '(0)
    [["Font Lock Levels"]
      [(transient-macro-call-treesit-set-font)]
      [(transient-macro-call-treesit-decrease-font)]
      [(transient-macro-call-treesit-increase-font)]
      ]
    )
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 06, 2025
;; Modified:   January 06, 2025
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
;;; transient-treesit.lsp ends here
