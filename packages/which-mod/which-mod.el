;;; which-mod.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 24, 2021
;; Modified: November 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
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

(message "Loading Which-mod")

(defun which-key--get-keymap-bindings-1
    (keymap start &optional prefix filter all ignore-commands)
  "See `which-key--get-keymap-bindings'."

  (let ((bindings start)
        (prefix-map (if prefix (lookup-key keymap prefix) keymap)))
    ;; Prefer which-key pseudo-maps:
    (when (keymapp prefix-map)
      (if (keymapp (lookup-key prefix-map [which-key]))
          (setq bindings (which-key--get-keymap-bindings-1 (lookup-key prefix-map [which-key])
                                                           nil nil filter all ignore-commands)))

      (map-keymap
       (lambda (ev def)
         (let* ((key (vconcat prefix (list ev)))
                (key-desc (key-description key)))
           (cond
            ((assoc (key-description (list ev)) bindings))
            ((assoc key-desc bindings))
            ((and (listp ignore-commands) (symbolp def) (memq def ignore-commands)))
            ((or (string-match-p
                  which-key--ignore-non-evil-keys-regexp key-desc)
                 (eq ev 'menu-bar)))
            ((and (keymapp def)
                  (string-match-p which-key--evil-keys-regexp key-desc)))
            ((and (keymapp def)
                  (or all
                      ;; event 27 is escape, so this will pick up meta
                      ;; bindings and hopefully not too much more
                      (and (numberp ev) (= ev 27))))
             (setq bindings
                   (which-key--get-keymap-bindings-1
                    keymap bindings key filter all ignore-commands)))
            (def
             (let* ((def (if (eq 'menu-item (car-safe def))
                             (which-key--get-menu-item-binding def)
                           def))
                    (binding
                     (cons key-desc
                           (cond
                            ((and (eq (car-safe def) 'which-key)
                                  (keymapp (cdr-safe def))))
                            ((and (eq (car-safe def) 'which-key)
                                  (not (caddr def)))
                             (s-append (cadr def) "+"))
                            ((eq (car-safe def) 'which-key)
                             (cadr def))
                            ((symbolp def) (which-key--compute-binding def))
                            ((keymapp def) "prefix")
                            ((eq 'lambda (car-safe def)) "+lambda")
                            ((eq 'closure (car-safe def)) "+closure")
                            ((stringp def) def)
                            ((vectorp def) (key-description def))
                            ((and (consp def)
                                  ;; looking for (STRING . DEFN)
                                  (stringp (car def)))
                             (concat (when (keymapp (cdr-safe def))
                                       "group:")
                                     (car def)))
                            (t "unknown")))))
               (when (and binding
                          (or (null filter)
                              (and (functionp filter)
                                   (funcall filter binding))))
                 (push binding bindings)))))))
       prefix-map))
   bindings))

(defun which-key--compute-binding (binding)
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

;; (setq which-key-key-replacement-alist
;;         (delete '("left" . "←") which-key-key-replacement-alist))
;; (setq which-key-key-replacement-alist
;;         (delete '("right" . "→") which-key-key-replacement-alist))

(provide 'which-mod)
;;; to_integrate.el ends here
