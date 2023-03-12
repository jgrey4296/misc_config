;;; general-mod.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 24, 2021
;; Modified: November 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/johngrey/general
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(message "Loading General Mod")


(define-advice general-extended-def-:which-key (:override (_state keymap key edef kargs)
                                                          +jg-general)
  " An alternative which-key implementation for General, using which-key's
which-key-add-keymap-based-replacements.

Add a which-key description for KEY.
If :major-modes is specified in EDEF, add the description for the corresponding
major mode. KEY should not be in the kbd format (kbd should have already been
run on it)."
    (let* ((wk (general--getf2 edef :which-key :wk))
           (keymaps (plist-get kargs :keymaps))
           (key (key-description key))
           (prefix (plist-get kargs :prefix))
           (binding (or (when (and (plist-get edef :def)
                                   (not (plist-get edef :keymp)))
                          (plist-get edef :def))
                        (when (and prefix
                                   (string= key prefix))
                          (plist-get kargs :prefix-command))))
           (replacement (cond ((consp wk) (cdr wk))
                              (t wk)))
           )
      (condition-case-unless-debug err
          (mapc #'(lambda (keymap-sym)
                    (cond ((and (boundp keymap-sym)
                                (keymapp (symbol-value keymap-sym)))
                           (general-which-key-add-evil-keymap-replacement _state (symbol-value keymap-sym)
                                                                          key `(,replacement . ,binding)))
                          ((and (boundp (intern (format "%s-map" keymap-sym)))
                                (keymapp (symbol-value (intern (format "%s-map" keymap-sym)))))
                           (general-which-key-add-evil-keymap-replacement _state (symbol-value (intern (format "%s-map" keymap-sym)))
                                                                          key `(,replacement . ,binding))
                           )
                          ))
                keymaps
                )
         (error (message "Binding Update Error for: (%s : %s : %s : %s) : %s" keymap key binding replacement err))
         )
    )
  )

(defun general-which-key--pseudo-key (key &optional prefix)
  "Replace the last key in the sequence KEY by a special symbol
in order for which-key to allow looking up a description for the key."
  (let ((seq (listify-key-sequence key)))
    (vconcat (or prefix (butlast seq)) [which-key] (last seq))))

(defun general-which-key-add-evil-keymap-replacement (state keymap key replacement &rest more)
  " Alt implementation of which-key-add-keymap-based-replacements
that uses evil-define-key, allowing state bindings

Mainly this is useful for a keymap-based-replacement implementation
of general-extended-def-:which-key
"
  (if (not (keymapp keymap))
      (error "Symbol is not a keymap" keymap))
  (while key
    (let* ((string (if (stringp replacement)
                       replacement
                     (car-safe replacement)))
           (command (cdr-safe replacement))
           (pseudo-key (general-which-key--pseudo-key (kbd key)))
           (bind `(which-key ,string ,command))
           )
      (if state
          (evil-define-key* state keymap pseudo-key bind)
        (define-key keymap pseudo-key bind)
        ))
    (setq key (pop more)
          replacement (pop more))))

;;(defalias 'general-extended-def-:wk #'general-extended-def-:which-key)

(provide 'general-mod)
;;; general.el ends here
