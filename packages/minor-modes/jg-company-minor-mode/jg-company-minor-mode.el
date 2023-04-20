 ;;; jg-company-minor-mode.el -*- lexical-binding: t; no-byte-compile: t;-*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 17, 2023
;; Modified: April 17, 2023
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
(require 'company)
;;-- end imports

;;-- vars
(defvar-local jg-company-minor-mode-activation-re nil)
(defvar-local jg-company-minor-mode-kws (make-hash-table :test 'equal))
;;-- end vars

;;-- mode definition

(define-minor-mode jg-company-minor-mode
  "  "
  :init-value nil
  :lighter "jg-company"
  ;; :global t
  ;; :keymap nil
  (unless (-contains? company-backends 'jg-company/backend)
    (message "Pushing to %s" company-backends)
    (push 'jg-company/backend company-backends)
    )
  )

(defun jg-company-minor-mode/turn-on ()
  (unless (minibufferp)
    (jg-company-minor-mode 1)
    )
  )

(define-globalized-minor-mode global-jg-company-minor-mode
  jg-company-minor-mode
  jg-company-minor-mode/turn-on
  (message "Activating global jg-company-minor-mode")
  )

;;-- end mode definition

(defun jg-company/backend (cmd &rest args)
  (interactive (list 'interactive))
  (cl-case cmd
    (init            nil)
    ;; Get the value to complete
    (prefix  (when (and (buffer-local-value jg-company-minor-mode-activation-re (current-buffer))
                        (s-matches? (buffer-local-value jg-company-minor-mode-activation-re (current-buffer)) (current-word)))
               (current-word)))
    ;; Get candidates of completion
    (candidates (gethash (current-word) (buffer-local-value jg-company-minor-mode-kws (current-buffer))))
    ;; Defaults
    (sorted          t)
    (duplicates      nil)
    (ignore-case     t)
    (no-cache        t)
    ;; Documentation location:
    (doc-buffer      nil)
    ;; Location of candidate definition
    (location        nil)
    ;; Add data in completion window
    (annotation      nil)
    ;; Add data in echo
    (meta            nil)
    ;; For Late expansion of snippets etc
    (post-completion nil)
    ;; For easy use of backend:
    (interactive     (company-begin-backend 'jg-company/backend))
    ;; Difference between usage / creation:
    (require-match   nil)

    (t               nil)
    )
  )

(provide 'jg-company-minor-mode)
;;; jg-company-minor-mode.el ends here
