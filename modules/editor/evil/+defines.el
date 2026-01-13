;;; +defines.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar +evil-repeat-keys (cons ";" ",")
  "The keys to use for universal repeating motions.

This is a cons cell whose CAR is the key for repeating a motion forward, and
whose CDR is for repeating backward. They should both be `kbd'-able strings.

Set this to `nil' to disable universal-repeating on these keys.")

(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")

(defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

(defvar jg-evil-surround-pairs-base
  '((?\( . ("( " . " )"))
    (?\[ . ("[ " . " ]"))
    (?\{ . ("{ " . " }"))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?# . ("#{" . "}"))
    (?b . ("(" . ")"))
    (?p . ("(" . ")"))
    (?B . ("{" . "}"))
    (?> . ("<" . ">"))
    )
  )

(defvar evil-textobj-anyblock-blocks
  '(("(" . ")")
    ("{" . "}")
    ("\\[" . "\\]")
    ("<" . ">"))
  )

;; --

(def-named-keymap! jge-insert-state-map)
(def-named-keymap! jge-replace-state-map)
(def-named-keymap! jge-motion-state-map)
(def-named-keymap! jge-normal-state-map)
(def-named-keymap! jge-operator-state-map)
(def-named-keymap! jge-visual-state-map)
(def-named-keymap! jge-insert-state-map)

(def-named-keymap! jge-b-op-motion-map)
(def-named-keymap! jge-f-op-motion-map)
(def-named-keymap! jge-inner-txtobj-map)
(def-named-keymap! jge-outer-txtobj-map)

(def-named-keymap! jge-helm-map)
(def-named-keymap! jge-operator-map)

;;; +defines.el ends here
