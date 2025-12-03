;;; +proof-general.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

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
  (add-hook! 'coq-mode-hook
             #'librarian-insert-minor-mode
             )

  (setq-hook! 'coq-mode-hook
    code-shy-fold-patterns (list "%s-- %s %s" "%s-- %s %s")
    code-shy-block-depth 1
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

(defvar jg-coq-mode-map (make-sparse-keymap))
(defvar jg-coq-proof-mode-map (make-sparse-keymap))

(after! proof-general
  (setq proof-splash-enable nil
        proof-three-window-enable nil
        coq-compile-before-require t
        coq-accept-proof-using-suggestion 'never
        )
  )

(setq-hook! 'coq-mode-hook
  ;; Doom syncs other indent variables with `tab-width'; we trust major modes to
  ;; set it -- which most of them do -- but coq-mode doesn't, so...
  tab-width proof-indent
  ;; HACK Fix #2081: Doom continues comments on RET, but coq-mode doesn't have a
  ;;      sane `comment-line-break-function', so...
  comment-line-break-function nil)

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
