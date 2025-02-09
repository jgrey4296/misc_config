;;; emoji.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
;; https://www.gnu.org/software//emacs/manual/html_node/elisp/Glyphless-Chars.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Glyphless-Chars.html

(setq glyphless-char-display (make-char-table 'glyphless))
(setq standard-display-table (make-display-table))

(set-char-table-range glyphless-char-display (char-from-name "VARIATION SELECTOR-16") 'empty-box)
(set-char-table-range standard-display-table (char-from-name "VARIATION SELECTOR-16") " ")
(set-char-table-range standard-display-table (char-from-name "WARNING SIGN") [?a])
(set-char-table-range standard-display-table (char-from-name "WARNING SIGN") nil)

(set-face-background 'glyphless-char "red")

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 03, 2025
;; Modified:   February 03, 2025
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
;;; emoji.el ends here

-
⚠️
-
⚠
