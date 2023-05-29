;;; lang/coq/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! (jg-bindings-total coq) "+bindings")

(use-package! proof-general
  :defer t
  :init
  (setq proof-splash-enable nil)
  :config
  (set-face-attribute 'proof-locked-face nil
                      :inverse-video t
                      :underline nil
                      )
  )


;;;###package coq
(setq-hook! 'coq-mode-hook
  ;; Doom syncs other indent variables with `tab-width'; we trust major modes to
  ;; set it -- which most of them do -- but coq-mode doesn't, so...
  tab-width proof-indent
  ;; HACK Fix #2081: Doom continues comments on RET, but coq-mode doesn't have a
  ;;      sane `comment-line-break-function', so...
  comment-line-break-function nil)

;; We've replaced coq-mode abbrevs with yasnippet snippets (in the snippets
;; library included with Doom).
(setq coq-mode-abbrev-table '())


;; This package provides more than just code completion, so we load it whether
;; or not :completion company is enabled.
(use-package! company-coq
  :defer t
  :hook (coq-mode . company-coq-mode)
  :config
  (setq company-coq-disabled-features '(hello company-defaults spinner))

  (if (modulep! :completion company)
      (define-key coq-mode-map [remap company-complete-common]
        #'company-indent-or-complete-common)
    ;; `company-coq''s company defaults impose idle-completion on folks, so
    ;; we'll set up company ourselves. See
    ;; https://github.com/cpitclaudel/company-coq/issues/42
    (add-to-list 'company-coq-disabled-features 'company))

  )
