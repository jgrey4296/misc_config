;;; emacs/bindings/+which-key-patch.el -*- lexical-binding: t; -*-

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
          (+jg-binding-which-key-add-keymap-based-evil-replacement _state real-keymap key `(,replacement . ,binding))
        (error (message "Binding Update Error for: (%s : %s : %s : %s) : %s" keymap key binding replacement err))
      )
    )
  )
)

(defun +jg-binding-which-key-add-keymap-based-evil-replacement (state keymap key replacement)
  " Alt implementation of which-key-add-keymap-based-replacements
that uses evil-define-key, allowing state bindings
"
    (let* ((string (if (stringp replacement)
                       replacement
                     (car-safe replacement)))
           (command (cdr-safe replacement))
           (pseudo-key (which-key--pseudo-key (kbd key)))
           (bind `(which-key ,(cons string command)))
          )
      (message "adding replacement: %s : %s" pseudo-key bind)
      (if state
          (evil-define-key* state keymap pseudo-key bind)
        (define-key keymap pseudo-key bind)
        )
      )
    )
(defalias 'general-extended-def-:which-key #'+jg-binding-general-which-key-handler)

;; for which-key:

(defun +jg-which-key--get-keymap-bindings (keymap &optional all prefix)
  " Upgrade of which-key--get-keymap-bindings to use
the newer which-key-add-keymap-based-replacements form

Return list of pairs
"
  (let (bindings)
    (map-keymap
     (lambda (ev def)
       (let* ((key (append prefix (list ev)))
              (key-desc (key-description key))
              (ignore-regexp (regexp-opt '("mouse-" "wheel-" "remap" "drag-" "scroll-bar"
                                           "select-window" "switch-frame" "<intercept-state>")))
              )
         (cond ((or (string-match-p
                     ignore-regexp key-desc)
                    (eq ev 'menu-bar)))
               ((eq (car key) 'which-key)
                (setq bindings (cl-remove-duplicates
                                (append bindings
                                        (which-key--get-keymap-bindings def all prefix))
                                :test (lambda (a b) (string= (car a) (car b))))))
               ;; extract evil keys corresponding to current state
               ((and (keymapp def)
                     (boundp 'evil-state)
                     (bound-and-true-p evil-local-mode)
                     (string-match-p (format "<%s-state>$" evil-state) key-desc))
                (setq bindings
                      ;; this function keeps the latter of the two duplicates
                      ;; which will be the evil binding
                      (cl-remove-duplicates
                       (append bindings
                               (which-key--get-keymap-bindings def all prefix))
                       :test (lambda (a b) (string= (car a) (car b))))))
               ((and (keymapp def)
                     (string-match-p which-key--evil-keys-regexp key-desc))
                ;;(message "Ignoring def because evil state isnt active key: %s : %s" key-desc def)
                )
               ((and (keymapp def)
                     (or all
                         ;; event 27 is escape, so this will pick up meta
                         ;; bindings and hopefully not too much more
                         (and (numberp ev) (= ev 27))))
                (setq bindings
                      (append bindings
                              (which-key--get-keymap-bindings def t key focus))))
               (t
                ;; TODO force which-key entries to override others
                ;; don't add binding if already defined
                (when def
                  (cl-pushnew
                   (cons key-desc
                         (cond
                          ((and (listp def) (eq (car def) 'which-key)) def)
                          ((keymapp def) "Prefix Command")
                          ((symbolp def) (copy-sequence (symbol-name def)))
                          ((eq 'lambda (car-safe def)) "lambda")
                          ((eq 'menu-item (car-safe def)) "menu-item")
                          ((stringp def) def)
                          ((vectorp def) (key-description def))
                          (t "unknown")))
                   bindings :test (lambda (a b) (string= (car a) (car b)))))))))
     keymap)
    bindings))

(defun +jg-which-key--get-pseudo-binding (key-bind-pair &optional prefix)
  (let* ((key (car key-bind-pair))
         (is-pseudo-binding (+jg-which-key-is-pseudo-bind-p (cdr key-bind-pair))))
    (when is-pseudo-binding
      `(,key . ,(caaddr key-bind-pair))
      )))

(defun +jg-which-key-is-pseudo-bind-p (bind)
  (and (listp bind) (eq (car bind) 'which-key))
)

(defun +jg-which-key--format-and-replace (unformatted &optional prefix preserve-full-key)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let ((sep-w-face
         (which-key--propertize which-key-separator
                                'face 'which-key-separator-face))
        (local-map (current-local-map))
        new-list)
    (dolist (key-bind-pair unformatted)
      (let* ((key (car key-bind-pair))
             (keys (if prefix
                       (concat (key-description prefix) " " key)
                     key))
             (updated-key-bind-pair (which-key--maybe-replace (cons keys (cdr key-bind-pair)) prefix))
             (kbd-repr (condition-case err
                           (kbd keys)
                         (error (kbd (format "<%s>" keys)))
                         ))
             (orig-desc (cdr updated-key-bind-pair))
             (group (which-key--group-p orig-desc))
             ;; At top-level prefix is nil
             (local (if kbd-repr (eq (which-key--safe-lookup-key local-map kbd-repr)
                                     (intern orig-desc))
                      nil))
             (hl-face (which-key--highlight-face orig-desc))
             (final-desc (which-key--propertize-description
                          orig-desc group local hl-face orig-desc))
             (key-seq (if preserve-full-key
                          (car updated-key-bind-pair)
                        (which-key--extract-key (car updated-key-bind-pair))))
             )
        (when final-desc
          (setq final-desc
                (which-key--truncate-description
                 (which-key--maybe-add-docstring final-desc orig-desc))))
        (when (consp updated-key-bind-pair)
          (push (list (which-key--propertize-key key-seq) sep-w-face final-desc)
                new-list)))
      )
    (nreverse new-list)))

(defun +jg-get-active-minor-modes ()
  (let* ((minor-modes minor-mode-list)
         (bound-vars (-filter #'boundp minor-modes))
         (active-minor-modes (-filter #'symbol-value bound-vars))
         )
    active-minor-modes
    )
  )

(defun +jg-consume-prefix-on-maps (prefix maps)
  " Take a prefix vector, and a list of maps
    lookup-key recursively, returning only keymaps
  "
  (let* ((current maps))
    (loop for pre across prefix do
          (setq current (mapcar #'(lambda (x) (if (keymapp x)
                                             (lookup-key x (key-description `(,pre)))))
                                current))
          )
    (-filter #'keymapp current)
    )
  )

(defun +jg-which-key--get-current-bindings (&optional prefix)
  "Generate a list of current active bindings."
  (message "Getting current bindings for: %s" prefix)
  (let* ((leader-map (if (string-prefix-p doom-leader-key (key-description prefix)) doom-leader-map))
         (major-mode-map-sym (intern (format "%s-map" major-mode)))
         (evil-state-map-sym (intern (format "evil-%s-state-map" evil-state)))
         ;; todo evil local maps, override, intercept
         (active-minor-modes (+jg-get-active-minor-modes))
         (minor-mode-maps (mapcar #'(lambda (x) (alist-get x minor-mode-map-alist))
                                                     active-minor-modes))
         (active-maps (-filter #'identity (seq-concatenate 'list (mapcar #'symbol-value (list major-mode-map-sym evil-state-map-sym))
                                                           minor-mode-maps)))
         ;; TODO handle prefix
         (prefix-handled (+jg-consume-prefix-on-maps prefix active-maps))
         (ignore-bindings '("self-insert-command" "ignore"
                            "ignore-event" "company-ignore"))
         unformatted bindings)

    (if leader-map
        (setq prefix-handled (append (+jg-consume-prefix-on-maps
                                      (vconcat (cdr (append prefix nil)))
                                      `(,leader-map))
                                     prefix-handled)))

    (setq unformatted (mapcar #'which-key--get-keymap-bindings prefix-handled))

    (loop for bind-pair in (-flatten-n 1 (-filter #'identity unformatted)) do
          (let* ((formatted (which-key--maybe-replace bind-pair prefix))
                 (key (car formatted))
                 (binding (cdr formatted))
                )
            (cond ((null key))
                  ((member binding ignore-bindings))
                  ((string-match-p which-key--ignore-keys-regexp key))
                  ((unless (assoc-string key bindings)
                     (push (cons key (which-key--compute-binding binding))
                           bindings))))))

    (nreverse bindings)
    )
  )


(defalias 'which-key--get-current-bindings #'+jg-which-key--get-current-bindings)
(defalias 'which-key--get-pseudo-binding #'+jg-which-key--get-pseudo-binding)
(defalias 'which-key--get-keymap-bindings #'+jg-which-key--get-keymap-bindings)
(defalias 'which-key--format-and-replace #'+jg-which-key--format-and-replace)
