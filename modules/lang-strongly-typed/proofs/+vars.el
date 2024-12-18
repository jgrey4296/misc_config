;;; +vars.el -*- lexical-binding: t; -*-

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
(speckler-add! lookup-handler ()
  `(company-coq-mode
    :definition ,#'company-coq-jump-to-definition
    :references ,#'company-coq-grep-symbol
    :documentation ,#'company-coq-doc
    )
  )
(speckler-add! auto-modes ()
  '(coq
    ("\\.v\\'" . coq-mode)
    )
  )
