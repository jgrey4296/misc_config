;;; +vars.el -*- lexical-binding: t; -*-

(defvar soar-comint-buffer-name "*soar*")

(defvar ceptre-repl-buffer-name "*ceptre*")

(defvar instal-repl-buffer-name "*instal*")

(defvar pasp-results-buffer-name "*clingo output*")

(defvar inferior-clips-buffer   nil)

(defvar soar-executable         "SoarCLI.sh")

(defvar clingo-executable       "clingo")

(defvar ceptre-executable       "ceptre")

(defvar instal-executable       "python -m instal")

(defvar inferior-clips-program  "CLIPS Console")

(spec-handling-add! eval
                    `(clips-mode  :start ,#'+clips-mode/open-repl)
                    `(instal-mode :start ,#'+instal-mode/open-repl)
                    `(soar-mode   :start ,#'+soar-mode/open-repl)
                    `(ceptre-mode :start ,#'+ceptre-mode/open-repl)
                    `(pasp-mode   :eval ,#'+jg-pasp-eval)
                    )

;;-- specs

(spec-handling-add! popup
                    `(clingo
                     (,(format "^\\*%s\\*" pasp-results-buffer-name) :side right :ttl 5 :width 0.4 :quit t :select nil :priority 50)
                     ("^\\*clingo output\\*" :side right :ttl 5 :width 0.4 :quit t :select nil)
                     )
                    )
(spec-handling-add! file-templates
                    '(logic
                     ("\\.lp4?"  :trigger "__" :mode pasp-mode)
                     ("\\.pl$"   :trigger "__" :mode prolog-mode)
                     ("\\.clp$"  :trigger "__" :mode clips-mode)
                     ("\\.asl$"  :trigger "__" :mode agentspeak-mode)
                     ("\\.cep$"  :trigger "__" :mode ceptre-mode)
                     ("\\.soar$" :trigger "__" :mode soar-mode)
                     )
                    )
(spec-handling-add! auto-modes
                    '(ai-and-logic
                      ("\\.pl\\'" . prolog-mode)
                      ("\\.lp\\'" . pasp-mode)
                      ("\\.lp4\\'" . pasp-mode)
                      ("\\.lp\\'" . pasp-mode)
                      ("\\.soar\\'" . soar-mode)
                      ("\\.\\(jcm\\|mas2j\\)\\'" . jacamo-mode)
                      ("\\.asl\\'" . agentspeak-mode)
                      ("\\.clp$" . clips-mode)
                      )
                    )
;;-- end specs
