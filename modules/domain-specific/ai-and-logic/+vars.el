;;; +vars.el -*- lexical-binding: t; -*-

(setq-default soar-repl-buffer-name "*soar*"
              clips-repl-buffer-name "*clips*"


              soar-executable "soar"
              inferior-clips-program "clips"
              )

(after! proof-general
    (setq proof-splash-enable nil
          proof-three-window-enable nil
          coq-compile-before-require t
          coq-accept-proof-using-suggestion 'never
          )
    (push 'coq-mode +jg-personal-major-modes)
)
