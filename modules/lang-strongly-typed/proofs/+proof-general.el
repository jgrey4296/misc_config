;;; +proof-general.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! proof-general
  :init
  (setq proof-splash-enable nil)
  :config
  (after! proof-faces
    (set-face-attribute 'proof-locked-face nil
                        :inverse-video nil
                        :underline t
                        )
    )
  (setq proof-splash-enable nil
        proof-three-window-enable nil
        coq-compile-before-require t
        coq-accept-proof-using-suggestion 'never
        )
  )

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

(setq-hook! 'coq-mode-hook
  tab-width proof-indent
  comment-line-break-function nil
  code-shy-fold-patterns (list "%s-- %s %s" "%s-- %s %s")
  code-shy-block-depth 1
  )

(add-hook! 'coq-mode-hook
           #'librarian-insert-minor-mode
           )

(speckler-setq! coq ()
  coq-prog-name "rocq"
  coq-prog-args '("repl")
  )
(speckler-add! popup ()
  '(coq
    ("^\\*\\(?:response\\|goals\\)\\*" :ignore t)
    )
  )
(speckler-add! doc-lookup ()
  `(company-coq-mode
    :definition    #'company-coq-jump-to-definition
    :references    #'company-coq-grep-symbol
    :documentation #'company-coq-doc
    )
  )
(speckler-add! auto-modes ()
  '(coq
    ("\\.v\\'" . coq-mode)
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 09, 2024
;; Modified:   September 09, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +proof-general.el ends here
