;;; emacs/bindings/+which-key-update.el -*- lexical-binding: t; -*-

(defun +jg-binding-kbd-heuristics (x)
  " Return True if input is a valid keybinding "
  (if (string-match jg-misc-ibuffer-heuristics (car x)) nil
    t))
(defun +jg-binding-update-guard (x)
  (condition-case-unless-debug err
      (let ((the-map (eval (car x))))
        (which-key-add-keymap-based-replacements
          the-map (cadr x) (caddr x)))
    (error (debug)))
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
(defun +jg-binding-process-triples (triple)
  " Convert which-key--get-bindings to a format
correct for which-key-add-keymap-based-replacements "
  (mapcar #'substring-no-properties
          (list (car triple) (caddr triple))))
(defun +jg-binding-keymap-update-plural (&rest the-maps)
  (mapcar #'+jg-binding-keymap-update-descs the-maps)
  )

(defun +jg-binding-finalise ()
  ;; Override Evil maps and use my own:
  (+jg-binding-keymap-update-plural 'jg-binding-operator-map
                                    'jg-binding-vision-map
                                    'jg-binding-forward-motion-map
                                    'jg-binding-backward-motion-map
                                    'jg-binding-inner-text-objects-map
                                    'jg-binding-outer-text-objects-map
                                    'jg-binding-normal-state-map
                                    'jg-binding-visual-state-map
                                    'jg-binding-operator-state-map
                                    'jg-binding-motion-state-map
                                    )

  (+jg-binding-keymap-update-plural 'evil-operator-state-map
                                    'evil-normal-state-map
                                    'evil-visual-state-map
                                    'evil-motion-state-map)

  (setq  evil-normal-state-map jg-binding-normal-state-map
         evil-visual-state-map jg-binding-visual-state-map
         evil-operator-state-map jg-binding-operator-state-map
         evil-motion-state-map jg-binding-motion-state-map
         )


  (setq evil-global-keymaps-alist
        '((evil-emacs-state-minor-mode    . evil-emacs-state-map)
          (evil-motion-state-minor-mode   . evil-motion-state-map)
          (evil-replace-state-minor-mode  . evil-replace-state-map)
          (evil-operator-state-minor-mode . evil-operator-state-map)
          (evil-visual-state-minor-mode   . evil-visual-state-map)
          (evil-insert-state-minor-mode   . evil-insert-state-map)
          (evil-normal-state-minor-mode   . evil-normal-state-map)))
  (message "Evil Bindings Complete")
)
