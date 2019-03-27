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
    (trie-mode :location local)
    (parsec :location elpa :step pre)
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
