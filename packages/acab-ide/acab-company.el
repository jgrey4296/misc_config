;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; TODO context aware company completion of rule names, layers,
;; operators etc
(require 'cl-lib)
(require 'company)

;; Applicable Contexts:
;; Rule/Layer/Pipeline/Ageda names
;;
;; Variables marked with $
;; Operators marked with λ
;; TYPES marked with ::
;; TAGS marked with #
;; The above ↑ could all be linked to acab config setup
;;
;; For a separate mode: FSM States, Game States, Agent Names,
;; String expansions...

(defun acab-company/backend (cmd &rest args)
  (cl-case cmd
    (init            nil)
    ;; Prefix Acab Company: check context, get line substring
    (prefix          nil)
    ;; Navigate down context db, return next children
    (candidates      nil)
    (sorted          t)
    (duplicates      t)
    (no-cache        nil)
    (ignore-case     t)
    (annotation      nil)
    (meta            nil)
    (location        nil)
    (post-completion nil)
    (require-match   nil)
    (t               nil)
    )
  )

(define-minor-mode acab-company-minor-mode
  " Minor Mode for Acab Company completion "
  :lighter "acab-company"
  :global t
  :keymap nil
  )
