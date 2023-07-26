;;; +defs.el -*- lexical-binding: t; -*-


(defvar +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode)
  "A list of major modes which should be highlighted by tree-sitter.

If this list begins with `not', then it negates the list.
If it is t, it is enabled in all modes.
If nil, it is disabled in all modes")



(defvar mouse-wheel-down-event nil)
(defvar mouse-wheel-up-event nil)
