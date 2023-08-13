;; +defs.el<2> -*- lexical-binding: t; -*-


(defvar +emacs-lisp-enable-extra-fontification t "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]" "Regexp to use for `outline-regexp' in `emacs-lisp-mode'. This marks a foldable marker for `outline-minor-mode' in elisp buffers.")

(defvar +emacs-lisp-linter-warnings
  '(not free-vars    ; don't complain about unknown variables
    noruntime    ; don't complain about unknown function calls
    unresolved)  ; don't complain about undefined functions
  "The value for `byte-compile-warnings' in non-packages.

This reduces the verbosity of flycheck in Emacs configs and scripts, which are
so stateful that the deluge of false positives (from the byte-compiler,
package-lint, and checkdoc) can be more overwhelming than helpful.

See `+emacs-lisp-non-package-mode' for details.")

(defvar jg-lisp-surround-pairs '((?t . evil-surround-read-tag)
                                 (?< . evil-surround-read-tag)
                                 (?f . evil-surround-prefix-function)
                                 (?F . evil-surround-function)
                                 )
  )
