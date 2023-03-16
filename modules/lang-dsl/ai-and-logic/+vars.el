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



;;-- eval handler
(set-eval-handler! '(pasp-mode) #'+jg-pasp-eval)
;;-- end eval handler

;;-- popup rules
(setq jg-clingo-popup-rules
      `((,(format "^\\*%s\\*" pasp-results-buffer-name) :side right :ttl 5 :width 0.4 :quit t :select nil :priority 50)
        ("^\\*clingo output\\*" :side right :ttl 5 :width 0.4 :quit t :select nil)
        )
      )
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'clingo jg-clingo-popup-rules)
  )
;;-- end popup rules

;;-- file templates
(after! jg-completion-templates
  ;; logic
  (+jg-completion-add-file-templates
   'logic
   '(
     ("\\.lp4?"  :trigger "__" :mode pasp-mode)
     ("\\.pl$"   :trigger "__" :mode prolog-mode)
     ("\\.clp$"  :trigger "__" :mode clips-mode)
     ("\\.asl$"  :trigger "__" :mode agentspeak-mode)
     ("\\.cep$"  :trigger "__" :mode ceptre-mode)
     ("\\.soar$" :trigger "__" :mode soar-mode)
     )
   )
  )
;;-- end file templates
