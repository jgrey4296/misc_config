;;; +vars.el -*- lexical-binding: t; -*-

(setq-default soar-comint-buffer-name "*soar*"
              ceptre-repl-buffer-name "*ceptre*"
              instal-repl-buffer-name "*instal*"

              inferior-clips-buffer   nil
              pasp-results-buffer-name "*clingo output*"


              soar-executable         "SoarCLI.sh"
              clingo-executable       "clingo"
              ceptre-executable       "ceptre"
              instal-executable       "python -m instal"
              inferior-clips-program  "CLIPS Console"
              )

;;-- coq
(after! proof-general
    (setq proof-splash-enable nil
          proof-three-window-enable nil
          coq-compile-before-require t
          coq-accept-proof-using-suggestion 'never
          )
    (push 'coq-mode +jg-personal-major-modes)
)

;;-- end coq

;;-- eval handler
(set-eval-handler! '(pasp-mode) #'+jg-pasp-eval)
;;-- end eval handler

;;-- popup rules
(setq jg-clingo-popup-rules
      '(((format "^\\*%s\\*" pasp-results-buffer-name) :side right :ttl 5 :width 0.4 :quit t :select nil :priority 50)
        )
      )
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'clingo jg-clingo-popup-rules)
  )
;;-- end popup rules
