;;; faster-whichkey.el -*- lexical-binding: t; -*-
(require 'general)
(require 'which-key)

(defvar faster-whichkey--current-bindings nil)

(defvar faster-whichkey-ignores-fns '(digit-argument))

(define-advice general-extended-def-:which-key (:override (_state keymap key edef kargs)
                                                          faster-whichkey)
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
                           (faster-whichkey-add-evil-keymap-replacement _state (symbol-value keymap-sym)
                                                                          key `(,replacement . ,binding)))
                          ((and (boundp (intern (format "%s-map" keymap-sym)))
                                (keymapp (symbol-value (intern (format "%s-map" keymap-sym)))))
                           (faster-whichkey-add-evil-keymap-replacement _state (symbol-value (intern (format "%s-map" keymap-sym)))
                                                                          key `(,replacement . ,binding))
                           )
                          ))
                keymaps
                )
         (error (message "Binding Update Error for: (%s : %s : %s : %s) : %s" keymap key binding replacement err))
         )
    )
  )

(define-advice which-key--get-keymap-bindings (:around (fn keymap &optional start &rest args) faster-whichkey)
  (setq faster-whichkey--current-bindings start)
  (apply fn keymap start args)
  (-filter #'(lambda (x) (s-present-p (cdr-safe x))) faster-whichkey--current-bindings)
  )

(define-advice which-key--get-keymap-bindings-1 (:override (keymap start &optional prefix filter all ignore-commands)
                                                           faster-whichkey)
  "See `which-key--get-keymap-bindings'."

  (let ((prefix-map (if prefix (lookup-key keymap prefix) keymap)))
    ;; Prefer which-key pseudo-maps:
    (when (and (keymapp prefix-map) (keymapp (lookup-key prefix-map [which-key])))
      (which-key--get-keymap-bindings-1 (lookup-key prefix-map [which-key]) nil nil nil all ignore-commands))

    (when (keymapp prefix-map)
        (map-keymap (-partial #'faster-whichkey-handle-binding prefix filter all ignore-commands) prefix-map))
    faster-whichkey--current-bindings
    )
  )

(define-advice which-key--compute-binding (:override (binding)
                                                     faster-whichkey)
  "Replace BINDING with remapped binding if it exists.

Requires `which-key-compute-remaps' to be non-nil"
  (let (remap)
    (cond ((and which-key-compute-remaps (setq remap (command-remapping binding)))
           (copy-sequence (symbol-name remap)))
          (t
           (copy-sequence (symbol-name binding))))))

(define-advice which-key--evil-operator-filter (:override (binding) faster-whichkey)
  (let ((def (cdr binding)))
    (and (functionp def)
         (not (evil-get-command-property def :suppress-operator))))
  )

(defun faster-whichkey-handle-binding (prefix filter all ignore-commands ev def)
  " main discriminator to add bindings to faster-whichkey--current-bindings "
  (let* ((key (vconcat prefix (list ev)))
         (key-desc (key-description key)))
    (cond
     ((assoc (key-description (list ev)) faster-whichkey--current-bindings)) ;; ignore raw binding that have already been set
     ((assoc key-desc faster-whichkey--current-bindings)) ;; ignore bindings that have already been set
     ((and (listp ignore-commands) (symbolp def) (memq def ignore-commands)) ;; add empty entry for ignored commands
      (push (cons key-desc "") faster-whichkey--current-bindings)
      )
     ((and (symbolp def) (memq def faster-whichkey-ignores-fns))
      (push (cons key-desc "") faster-whichkey--current-bindings)
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
      (let ((binding (cons key-desc (faster-whichkey-handle-def def))))
        (when (and binding
                   (or (null filter) (and (functionp filter) (funcall filter (cons key-desc def)))))
          (push binding faster-whichkey--current-bindings))
        )
      )
     )
    )
  )

(defun faster-whichkey-handle-def (def)
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

(defun faster-whichkey-add-keymap-replacement (state keymap key replacement &rest more)
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
           (bind (faster-whichkey--build-pseudo-binding string command))
           )
      ;;(message "adding replacement: %s : %s" pseudo-key bind)
      (if state
          (evil-define-key* state keymap pseudo-key bind)

(define-key keymap pseudo-key bind)
        ))
    (setq key (pop more)
          replacement (pop more))))

(defun faster-whichkey--pseudo-key (key &optional prefix)
  " create a pseudo-keystring to target which-key information
ie: [SPC d f] -> [SPC d whichkey f]
 "
  (let ((seq (listify-key-sequence key)))
    (vconcat (or prefix (butlast seq)) [which-key] (last seq))))

(defun faster-whichkey--build-pseudo-binding (desc bind)
  "create a pseudo binding to hold a which-key description"
  (list 'which-key desc bind)
  )

(defun faster-whichkey-add-evil-keymap-replacement (state keymap key replacement &rest more)
  " Alt implementation of faster-whichkey-add-keymap-based-replacements
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
           (pseudo-key (faster-whichkey--pseudo-key (kbd key)))
           (bind `(which-key ,string ,command))
           )
      (if state
          (evil-define-key* state keymap pseudo-key bind)

(define-key keymap pseudo-key bind)
        ))
    (setq key (pop more)
          replacement (pop more))))

(provide 'faster-whichkey)
