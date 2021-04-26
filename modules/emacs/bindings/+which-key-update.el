;;; emacs/bindings/+which-key-update.el -*- lexical-binding: t; -*-

(defun +jg-binding-process-triples (triple)
  " Convert which-key--get-bindings to a format
correct for which-key-add-keymap-based-replacements "
  (mapcar #'substring-no-properties
          (list (car triple) (caddr triple))))

(defun +jg-binding-kbd-heuristics (x)
  " Return True if input is a valid keybinding "
  (if (string-match jg-misc-ibuffer-heuristics (car x)) nil
    t))

(defun +jg-binding-update-guard (x)
  (condition-case-unless-debug err
      (let ((the-map (eval (car x))))
        (which-key-add-keymap-based-replacements
          the-map (cadr x) (caddr x)))
    (error (message "Binding Update Error: %s" err)))
  )

(defun +jg-binding-keymap-update-prefixs (the-map)
   " Update which-key descriptions for a keymap "
  (let* ((curr-map (eval the-map))
         (triples (which-key--get-bindings nil curr-map))
         (pairs (mapcar #'+jg-binding-process-triples triples))
         (prefix-p #'(lambda (x) (string-match "^\+" (cadr x))))
         (filtered (-filter prefix-p pairs))
         (reduced (mapcar #'(lambda (x) (list (car x) (replace-regexp-in-string "^\++" "" (cadr x)))) filtered))
         (with-map-pairs (mapcar #'(lambda (x) (cons the-map x)) reduced))
         )
    (mapc #'+jg-binding-update-guard with-map-pairs)
    )
  )

(defun +jg-binding-keymap-update-descs (the-map)
  " Update which-key descriptions for a keymap "
  (message "Updating Descriptions for: %s" the-map)
  (let* ((curr-map (eval the-map))
         (triples (which-key--get-bindings nil curr-map nil t))
         (pairs (mapcar #'+jg-binding-process-triples triples))
         (filtered (-filter #'+jg-binding-kbd-heuristics pairs))
         (with-map-pairs (mapcar #'(lambda (x) (cons the-map x)) filtered))
         )
    ;; (apply #'which-key-add-keymap-based-replacements the-map
    ;; (flatten-list filtered))

    (mapc #'+jg-binding-update-guard with-map-pairs)
    )
  )

(defun +jg-binding-keymap-update-plural (&rest the-maps)
  (mapcar #'+jg-binding-keymap-update-descs the-maps)
  )


;; For which General extension:
;;general-extended-def-keywords
(defun +jg-binding-general-which-key-handler (_state keymap key edef kargs)
  " An alternative which-key implementation for General, using which-key's
which-key-add-keymap-based-replacements.

Add a which-key description for KEY.
If :major-modes is specified in EDEF, add the description for the corresponding
major mode. KEY should not be in the kbd format (kbd should have already been
run on it)."
  (general-with-eval-after-load 'which-key
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
           ;;(formatted-repl (car (which-key--format-and-replace `((,key . ,replacement)))))
           (real-keymap (if (boundp keymap) (symbol-value keymap) (symbol-value (intern (format "%s-map" keymap)))))
           )
      (condition-case-unless-debug err
            (which-key-add-keymap-based-replacements real-keymap key replacement)
        (error (message "Binding Update Error for: (%s : %s : %s) : %s" keymap key binding replacement err))
      )
    )
  )
)
