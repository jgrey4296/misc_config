;;; lookup-regular.el -*- lexical-binding: t; -*-

 ;;; lookup-regular.el -*- lexical-binding: t; no-byte-compile: t;-*-
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
;;-- end imports

;;-- vars
(defvar lookup-regular-targets '())
(defvar lookup-regular-minor-mode-map (make-sparse-keymap))
;;-- end vars

;;-- mode definition

;;;###autoload
(define-minor-mode lookup-regular-minor-mode
  "  "
  :init-value nil
  :lighter "lookup-regular"
  ;; :global t
  :keymap lookup-regular-minor-mode-map
  )

;;;###autoload
(defun lookup-regular-minor-mode/turn-on ()
  (unless (minibufferp)
    (if (eq major-mode 'prog-mode)
        (lookup-regular-minor-mode 1))
    )
  )

;;;###autoload
(define-globalized-minor-mode global-lookup-regular-minor-mode lookup-regular-minor-mode lookup-regular-minor-mode/turn-on)

;;-- end mode definition

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
