;;; +vars.el -*- lexical-binding: t; -*-

(setq-default soar-repl-buffer-name   "*soar*"
              clingo-repl-buffer-name "*clingo*"
              ceptre-repl-buffer-name "*ceptre*"
              instal-repl-buffer-name "*instal*"
              inferior-clips-buffer   nil


              soar-executable         "SoarCLI.sh"
              clingo-executable       "clingo"
              ceptre-executable       "ceptre"
              instal-executable       "python -m instal"
              inferior-clips-program  "CLIPS Console"
              )

(after! proof-general
    (setq proof-splash-enable nil
          proof-three-window-enable nil
          coq-compile-before-require t
          coq-accept-proof-using-suggestion 'never
          )
    (push 'coq-mode +jg-personal-major-modes)
)
