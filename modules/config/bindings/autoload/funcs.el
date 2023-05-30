;;; util/bindings/+funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bindings-wk-filter-fn (binding)
  (not (string-match (rx (or "C-"
                             "C-M"
                             "M-"
                             ;; "s-"
                             ))
                     (car binding)))
  )

;;;###autoload
(defun +jg-binding-change-ext ()
  (interactive)
  (let* ((current (buffer-file-name))
        (curr-ext (f-ext current))
        (newext  (read-string (format "Extension %s -> ." curr-ext)))
        )
    (message "Converting %s -> %s" current (f-swap-ext current newext))
    (rename-file current (f-swap-ext current newext))
    )
  )

;;;###autoload
(defun +jg-bindings-check-map (the-map)
  " Take a keymap and print out all meta keys of the map "
  (cl-assert (keymapp the-map))
  (let ((c-xs (cl-loop for key in (number-sequence ?a ?z)
                       when (lookup-key the-map (kbd (format "C-%s" (char-to-string key))))
                       collect
                       (format "C-%s" (char-to-string key))))
        (m-xs (cl-loop for key in (number-sequence ?a ?z)
                       when (lookup-key the-map (kbd (format "M-%s" (char-to-string key))))
                       collect
                       (format "M-%s" (char-to-string key))))
        (cm-xs (cl-loop for key in (number-sequence ?a ?z)
                       when (lookup-key the-map (kbd (format "C-M-%s" (char-to-string key))))
                       collect
                       (format "C-M-%s" (char-to-string key))))
        ;; todo: also handle S, and mouse-1, mouse-2
        )

    (message "C-?'s: ")
    (mapc (lambda (x) (message "%s " x)) c-xs)
    (message "--------------------")
    (message "M-?'s: ")
    (mapc (lambda (x) (message "%s" x)) c-xs)
    (message "--------------------")
    (message "C-M-?'s: ")
    (mapc (lambda (x) (message "%s" x)) c-xs)
    (list c-xs m-xs cm-xs)
    )
  )

;;;###autoload
(defun +jg-bindings-undefine-metas (the-map)
  (cl-assert (keymapp the-map))
  (cl-loop for acc-map in (accessible-keymaps the-map)
        do
        (if (not (keymapp acc-map))
            (setq acc-map (cdr acc-map)))
        (cl-loop for x in (number-sequence ?a ?z)
                do
                (let ((fmt (char-to-string x)))
                  (if (lookup-key acc-map (kbd (format "C-%s" fmt)))
                      (define-key acc-map (kbd (format "C-%s" fmt)) nil)
                    )
                  (if (and (keymapp acc-map)
                           (lookup-key acc-map (kbd (format "C-M-%s" fmt))))
                      (define-key acc-map (kbd (format "C-M-%s" fmt)) nil)
                    )
                  (if (lookup-key acc-map (kbd (format "M-%s" fmt)))
                      (define-key acc-map (kbd (format "M-%s" fmt)) nil)
                    )
                  )
                )
        (if (lookup-key acc-map '[menu-bar])
            (define-key acc-map '[menu-bar] nil))
        (if (lookup-key acc-map '[mouse-1])
            (define-key acc-map '[mouse-1] nil))
        (if (lookup-key acc-map '[mouse-2])
            (define-key acc-map '[mouse-2] nil))
        (if (lookup-key acc-map '[mouse-3])
            (define-key acc-map '[mouse-3] nil))
        (if (lookup-key acc-map '[mouse-4])
            (define-key acc-map '[mouse-4] nil))
        (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                              (delete x acc-map))) acc-map)
        )
  )

;;;###autoload
(defun +jg-bindings-all-maps ()
  (interactive)
  (let (allmaps)
    (cl-do-symbols (sym)
      (when (or (keymapp sym) (and (s-matches? "-map$" (symbol-name sym)) (not (functionp sym))))
        (push sym allmaps)
        )
      )
    (message "There are %s keymaps" (length allmaps))
    allmaps
    )
  )
