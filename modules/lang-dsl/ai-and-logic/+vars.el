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

;;-- popup spec
(after! jg-ui-reapply-hook-ready
  (+jg-popup-add-spec 'clingo
                      `(
                        (,(format "^\\*%s\\*" pasp-results-buffer-name) :side right :ttl 5 :width 0.4 :quit t :select nil :priority 50)
                        ("^\\*clingo output\\*" :side right :ttl 5 :width 0.4 :quit t :select nil)
                        )
                      )
  )
;;-- end popup spec

;;-- file spec
(after! jg-ui-reapply-hook-ready
  ;; logic
  (+jg-snippets-add-file-spec 'logic
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
;;-- end file spec
