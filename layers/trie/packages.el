;; trie packages.el
;; loads second

(defconst trie-packages
  '(
    ;; package from EPA
    ;; eg: some-package
    ;; (some-package :location elpa)
    ;; (some-package :location local)
    ;; (some-package :location (recipe :fetcher github :repo "some/repo"))
    ;; (some-package :excluded t)
    org
    (trie-mode :location local)
    (parsec :location elpa :step pre)
    (sequence-mode :location local)
    )
  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)
;; Use:
;; (use-package 'name :commands :config ...
(defun trie/init-trie-mode ()
  (use-package trie-mode))

(defun trie/init-parsec ()
  (use-package parsec
    :defer t))


(defun trie/init-sequence-mode ()
  (use-package sequence-mode
    :config
    (spacemacs/declare-prefix "," "Sequence Mode Prefix")
    (evil-define-key '(normal visual) sequence-mode-map
      "l" 'sequence/user-inc-column
      "h" 'sequence/user-dec-column
      "k" 'sequence/user-dec-line
      "j" 'sequence/user-inc-line
      )
    (spacemacs/set-leader-keys-for-major-mode 'sequence-mode
      "."   'spacemacs/sequence_transient-transient-state/body
      )
    )
)
