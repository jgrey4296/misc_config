;;; which-mod.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 24, 2021
;; Modified: November 24, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/johngrey/to_integrate
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'which-key)
(message "Loading Which-mod")

(defvar which-mod-bindings nil)

(defvar which-mod-ignores-fns '(digit-argument))

(define-advice which-key--get-keymap-bindings (:around (fn keymap &optional start &rest args) which-mod-init)
  (setq which-mod-bindings start)
  (apply fn keymap start args)
  (-filter #'(lambda (x) (s-present-p (cdr-safe x))) which-mod-bindings)
  )

(define-advice which-key--get-keymap-bindings-1 (:override (keymap start &optional prefix filter all ignore-commands)
                                                           which-mod-main)
  "See `which-key--get-keymap-bindings'."

  (let ((prefix-map (if prefix (lookup-key keymap prefix) keymap)))
    ;; Prefer which-key pseudo-maps:
    (when (and (keymapp prefix-map) (keymapp (lookup-key prefix-map [which-key])))
      (which-key--get-keymap-bindings-1 (lookup-key prefix-map [which-key]) nil nil nil all ignore-commands))

    (when (keymapp prefix-map)
        (map-keymap (-partial #'which-mod-handle-binding prefix filter all ignore-commands) prefix-map))
    which-mod-bindings
    )
  )

(defun which-mod-handle-binding (prefix filter all ignore-commands ev def)
  " main discriminator to add bindings to which-mod-bindings "
  (let* ((key (vconcat prefix (list ev)))
         (key-desc (key-description key)))
    (cond
     ((assoc (key-description (list ev)) which-mod-bindings)) ;; ignore raw binding that have already been set
     ((assoc key-desc which-mod-bindings)) ;; ignore bindings that have already been set
     ((and (listp ignore-commands) (symbolp def) (memq def ignore-commands)) ;; add empty entry for ignored commands
      (push (cons key-desc "") which-mod-bindings)
      )
     ((and (symbolp def) (memq def which-mod-ignores-fns))
      (push (cons key-desc "") which-mod-bindings)
      )
     ((or (string-match-p which-key--ignore-non-evil-keys-regexp key-desc) (eq ev 'menu-bar)) ;; ignoring extra stuff
      nil )
     ((and (keymapp def) (string-match-p which-key--evil-keys-regexp key-desc)) ;; ignoring evil states
      nil)
     ((and (keymapp def) (or all (and (numberp ev) (= ev 27)))) ;; event 27 is escape, so this will pick up meta
      (which-key--get-keymap-bindings-1 keymap nil key filter all ignore-commands))
     ((eq 'menu-item (car-safe def)) ;; ignore menu items (which-key--get-menu-item-binding def)
      nil)
     (def
      (let ((binding (cons key-desc (which-mod-handle-def def))))
        (when (and binding
                   (or (null filter) (and (functionp filter) (funcall filter (cons key-desc def)))))
          (push binding which-mod-bindings))
        )
      )
     )
    )
  )

(defun which-mod-handle-def (def)
  " handler for defs "
  (cond
   ((and (eq (car-safe def) 'which-key) (keymapp (cdr-safe def))) ;; ignore which-keys that are submaps without names
    nil)
   ((and (eq (car-safe def) 'which-key) (not (caddr def)))
    (s-prepend "++" (cadr def))) ;; ++submap name
   ((eq (car-safe def) 'which-key) ;; described binding
    (cadr def))
   ((symbolp def) ;; remapped binding
    (which-key--compute-binding def))
   ((keymapp def) "prefix") ;; unnamed submap
   ((eq 'lambda (car-safe def)) "+lambda") ;; unnamed lambda
   ((eq 'closure (car-safe def)) "+closure") ;; unnamed closure
   ((stringp def) def)
   ((vectorp def) (key-description def))
   ((and (consp def) (stringp (car def))) ;; looking for (STRING . DEFN)
    (concat (when (keymapp (cdr-safe def)) "group:")
            (car def)))
   (t "unknown"))
  )

(define-advice which-key--compute-binding (:override (binding)
                                                     +jg-which-key)
  "Replace BINDING with remapped binding if it exists.

Requires `which-key-compute-remaps' to be non-nil"
  (let (remap)
    (cond ((and which-key-compute-remaps (setq remap (command-remapping binding)))
           (copy-sequence (symbol-name remap)))
          (t
           (copy-sequence (symbol-name binding))))))

(defun which-key--build-pseudo-binding (desc bind)
  " Build a pseudo-binding list for adding to a keymap "
  `(which-key ,desc ,bind))

(defun which-key-add-keymap-replacement (state keymap key replacement &rest more)
  " Alt implementation of which-key-add-keymap-based-replacements
that uses evil-define-key, allowing state bindings

Mainly this is useful for a keymap-based-replacement implementation
of general-extended-def-:which-key
"
  (cl-assert (keymapp keymap))
  (while key
    (let* ((string (if (stringp replacement)
                       replacement
                     (car-safe replacement)))
           (command (cdr-safe replacement))
           (pseudo-key (which-key--pseudo-key (kbd key)))
           (bind (which-key--build-pseudo-binding string command))
           )
      ;;(message "adding replacement: %s : %s" pseudo-key bind)
      (if state
          (evil-define-key* state keymap pseudo-key bind)
        (define-key keymap pseudo-key bind)
        ))
    (setq key (pop more)
          replacement (pop more))))

(define-advice which-key--evil-operator-filter (:override (binding) which-mod)
  (let ((def (cdr binding)))
    (and (functionp def)
         (not (evil-get-command-property def :suppress-operator)))))

;; (setq which-key-key-replacement-alist
;;         (delete '("left" . "←") which-key-key-replacement-alist))
;; (setq which-key-key-replacement-alist
;;         (delete '("right" . "→") which-key-key-replacement-alist))

(provide 'which-mod)
;;; to_integrate.el ends here
