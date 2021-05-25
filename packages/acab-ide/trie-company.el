;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; https://github.com/company-mode/company-mode/wiki/Writing-backends
;;
(require 'cl-lib)
(require 'company)
(provide 'trie-company)

(defvar trie-company/completion-trie nil)

(defun trie-company/backend (cmd &rest args)
  (message "Trie Company Backend: %s : %s" cmd args)
  (cl-case cmd
    (init            nil)
    (prefix          nil)
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


;; TODO provide company completion based on position in a trie
(define-minor-mode trie-company-minor-mode
 "This minor mode enables completion of trie sentences using company."
  :init-value nil
  :global t
  (cond
   (trie-company-minor-mode
    (setq company-backends
          (add-to-list 'company-backends 'trie-company/backend))
    (setq company-selection-default nil))
   (t
    (setq company-backends
          (delete 'trie-company/backend company-backends))
    (setq company-selection-default 0))))
