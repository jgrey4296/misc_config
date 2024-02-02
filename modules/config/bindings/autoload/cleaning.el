;;; cleaning.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

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
  " For a keymap, remove all bindings of the form:
  C-[a-z] C-M-[a-z] M-[a-z]
  # TODO: undefine pinch,
  "
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
        (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                              (delete x acc-map))) acc-map)
        )
  )

;;;###autoload
(defun +jg-bindings-undefine-mouse (kmap)
  " TODO For a keymap, remove all bindings using the mouse:
  [mouse-1] [mouse-2] [mouse-3] [mouse-4]
  S-mouse, wheel-down/up, drag-mouse, down-mouse,
  "
  (cl-assert (keymapp kmap))
  (cl-loop for acc-map in (accessible-keymaps kmap)
           do
           (if (not (keymapp acc-map))
               (setq acc-map (cdr acc-map)))
           (if (lookup-key acc-map '[mouse-1])
               (define-key acc-map '[mouse-1] nil))
           (if (lookup-key acc-map '[mouse-2])
               (define-key acc-map '[mouse-2] nil))
           (if (lookup-key acc-map '[mouse-3])
               (define-key acc-map '[mouse-3] nil))
           (if (lookup-key acc-map '[mouse-4])
               (define-key acc-map '[mouse-4] nil))
           )

           (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                                 (delete x acc-map))) acc-map)
  )

;;;###autoload
(defun +jg-bindings-undefine-menu-bar (kmap)
  " TODO remove menu/tab/tool-bar bindings "
  (cl-assert (keymapp kmap))
  (cl-loop for acc-map in (accessible-keymaps kmap)
           do
           (if (not (keymapp acc-map))
               (setq acc-map (cdr acc-map)))
           (if (lookup-key acc-map '[menu-bar])
               (define-key acc-map '[menu-bar] nil))

           (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                                 (delete x acc-map))) acc-map)
           )
  )

;;;###autoload
(defun +jg-bindings-undefine-f-keys (kmap)
  " TODO remove f1-f9 bindings "
  (cl-assert (keymapp kmap))
  (cl-loop for acc-map in (accessible-keymaps kmap)
           do
           (if (not (keymapp acc-map))
               (setq acc-map (cdr acc-map)))
           (if (lookup-key acc-map '[menu-bar])
               (define-key acc-map '[menu-bar] nil))

           (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                                 (delete x acc-map))) acc-map)
           )
  )

;;;###autoload
(defun +jg-bindings-all-maps ()
  " Get a list of all keymaps "
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

;;;###autoload
(defun +jg-bindings-name-all-maps ()
  " Loop over all symbols and get any that are keymaps,
    Message the names of all those maps
 "
  (interactive)
  (let ((maps (current-active-maps))
        (mapcount 0)
        all-maps
        )
    (cl-do-all-symbols (sym)
      (when (and (keymapp (ffap-symbol-value sym))
                 (sequencep (ffap-symbol-value sym))
                 )
        (cl-incf mapcount)
        (push (symbol-name sym) all-maps)
        (set sym (append (symbol-value sym) `((:name ,sym))))
        )
      )

    (message "Map Names: %s" all-maps)
    (message "There are %s active maps\nThere are %s maps total" (length maps) mapcount)
    nil
    )
  )
;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 29, 2024
;; Modified:   January 29, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; cleaning.el ends here
