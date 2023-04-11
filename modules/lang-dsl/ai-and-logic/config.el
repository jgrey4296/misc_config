;; Misc AI Languages:


(load! "+indent")
(load! "+vars")
(load! "+repl")
(load! "+funcs")
(load! "+advice")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(use-package! agentspeak-mode :defer t :mode ("\\.asl" . agentspeak-mode) )
(use-package! instal-mode     :defer t)
(use-package! jacamo-mode     :defer t :mode ("\\.\\(jcm\\|mas2j\\)" . jacamo-mode) )
(use-package! soar-mode       :defer t :mode ("\\.soar" . soar-mode) )
(use-package! clips-mode      :defer t)

(after! org
  (push '("clingo" . prolog) org-src-lang-modes)
  (push '("ccalc" . prolog) org-src-lang-modes)
  )

(after! ob-prolog
  (setq org-babel-prolog-command "swipl")
  )

(use-package! pasp-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.lp4" . pasp-mode ))
  (add-to-list 'auto-mode-alist '("\\.lp$" . pasp-mode))
  (add-hook 'pasp-mode-hook (lambda ()
                              (setq-local indent-line-function '+jg-logic-pasp-indent)))

  )
