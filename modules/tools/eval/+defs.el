;;; +defs.el -*- lexical-binding: t; no-byte-compile: t; -*-


(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl-other-window' and filled with the `:start' setting.")

(defvar +eval-repl-buffer-name "*repl*")

(defvar +eval-popup-min-lines 4
  "The output height threshold (inclusive) before output is displayed in a popup
buffer rather than an overlay on the line at point or the minibuffer.")


;;; +defs.el ends here
