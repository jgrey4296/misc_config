;;; lookup-regular.el -*- lexical-binding: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 20, 2023
;; Modified: April 20, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;-- end header

;;-- imports
(require 'ivy)
(require 'browse-url)
(require 'f)
(require 'cl-lib)
;;-- end imports

;;-- vars
(defvar-local lookup-regular-targets nil)
(defvar lookup-regular-minor-mode-map (make-sparse-keymap))
(defvar lookup-regular-location nil)
(defvar lookup-regular--cache (make-hash-table))
(defconst lookup-regular-splitter "#")
;;-- end vars

;;-- mode definition

;;;###autoload
(define-minor-mode lookup-regular-minor-mode
  " for all modes in (parent-mode-list major-mode) load any
files of urls in lookup-regular-location "
  :init-value nil
  :lighter "lookup-regular"
  ;; :global t
  :keymap lookup-regular-minor-mode-map
  (setq-local lookup-regular-targets
              (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                       when (f-exists? (f-join lookup-regular-location (symbol-name mode)))
                       do
                       (unless (gethash mode lookup-regular--cache)
                         (puthash mode (lookup-regular--load-file (f-join lookup-regular-location (symbol-name mode)))
                                  lookup-regular--cache)
                         )
                       append
                       (gethash mode lookup-regular--cache)
                       )
              )
  )

(defun lookup-regular--load-file (file)
  "read a list of (name . url) from the given file"
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (mapc #'(lambda (x)
                (-when-let (vals (split-string x lookup-regular-splitter t " +"))
                  (push (cons (car vals) (cadr vals)) targets)
                  ))
            (s-lines (buffer-substring-no-properties (point-min) (point-max)))
            )
      )
    targets
    )
  )

;;;###autoload
(defun lookup-regular-minor-mode/turn-on ()
  (unless (minibufferp)
    (lookup-regular-minor-mode 1))
  )

;;;###autoload
(define-globalized-minor-mode global-lookup-regular-minor-mode lookup-regular-minor-mode lookup-regular-minor-mode/turn-on)

;;-- end mode definition

;;;###autoload
(defun lookup-regular-go ()
  (interactive)
  (ivy-read "Lookup: "
            (buffer-local-value 'lookup-regular-targets (current-buffer))
            :require-match t :sort t
            :action #'(lambda (x) (browse-url (cdr x)))
            )
)

(provide 'lookup-regular)
;;; lookup-regular.el ends here
