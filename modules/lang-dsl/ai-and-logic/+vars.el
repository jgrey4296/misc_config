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

;;-- eval handler
(set-eval-handler! '(pasp-mode) #'+jg-pasp-eval)
;;-- end eval handler

;;-- specs

(spec-handling-add! popup nil
                    `(clingo
                     (,(format "^\\*%s\\*" pasp-results-buffer-name) :side right :ttl 5 :width 0.4 :quit t :select nil :priority 50)
                     ("^\\*clingo output\\*" :side right :ttl 5 :width 0.4 :quit t :select nil)
                     )
                    )
(spec-handling-add! file-templates nil
                    '(logic
                     ("\\.lp4?"  :trigger "__" :mode pasp-mode)
                     ("\\.pl$"   :trigger "__" :mode prolog-mode)
                     ("\\.clp$"  :trigger "__" :mode clips-mode)
                     ("\\.asl$"  :trigger "__" :mode agentspeak-mode)
                     ("\\.cep$"  :trigger "__" :mode ceptre-mode)
                     ("\\.soar$" :trigger "__" :mode soar-mode)
                     )
                    )
(spec-handling-add! lookup-regular nil
                    '(jacamo-mode
                     ("Jacamo Github" . "https://github.com/jacamo-lang/jacamo")
                     ("Jacamo API" . "https://jacamo.sourceforge.net/doc/api/index.html")
                     ("JASON Api" . "https://jason.sourceforge.net/api/index.html")
                     ("JASON Github" . "https://github.com/jason-lang/jason")
                     )
                    '(prolog-mode
                     ("SWIPL reference" . "https://www.swi-prolog.org/pldoc/doc_for?object=manual")
                     ("SWIPL CLI" . "https://www.swi-prolog.org/pldoc/man?section=cmdline")
                     )
                    '(soar-mode
                     ("Soar Reference" . "https://soar.eecs.umich.edu/articles/articles/documentation/73-command-line-help")
                     )
                    '(pasp-mode
                     ("Potassco homepage" . "https://potassco.org/doc/")
                     )
                    '(z3-mode
                     ("Z3 github" . "https://github.com/Z3Prover/z3")
                     ("Z3 Guide" . "https://z3prover.github.io/papers/programmingz3.html")
                     ("Z3 internals" . "https://z3prover.github.io/papers/z3internals.html")
                     ("F* Z3 guide" . "http://fstar-lang.org/tutorial/book/under_the_hood/uth_smt.html#understanding-how-f-uses-z3")
                     ("Z3 Tutorial" . "https://www.philipzucker.com/z3-rise4fun/guide.html")
                     )
                    )
;;-- end specs
