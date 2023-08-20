;;; lang/coq/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! (jg-bindings-total coq) "+bindings")

;;;###package coq
(use-package! proof-general
  :commands (proof-mode proofgeneral coq-mode)
  :init
  (setq proof-splash-enable nil)
  :config
  (set-face-attribute 'proof-locked-face nil
                      :inverse-video t
                      :underline nil
                      )
  )


;; This package provides more than just code completion, so we load it whether
;; or not :completion company is enabled.
(use-package! company-coq
  :after coq-mode
  :hook (coq-mode . company-coq-mode)
  :config
  (setq company-coq-disabled-features '(hello company-defaults spinner))

  (define-key coq-mode-map [remap company-complete-common] #'company-indent-or-complete-common)
  ;; `company-coq''s company defaults impose idle-completion on folks, so
  ;; we'll set up company ourselves. See
  ;; https://github.com/cpitclaudel/company-coq/issues/42
  (add-to-list 'company-coq-disabled-features 'company)
  )
