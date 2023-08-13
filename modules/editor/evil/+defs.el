;;; +defs.el -*- lexical-binding: t; -*-

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

;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)  ; we do this ourselves
(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)


(defvar jg-evil-surround-pairs-base '((?\( . ("( " . " )"))
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

(defvar evil-textobj-anyblock-blocks '(("(" . ")")
                                       ("{" . "}")
                                       ("\\[" . "\\]")
                                       ("<" . ">"))
  )
