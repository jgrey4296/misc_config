;; Misc AI Languages:


(load! "+indent")
(load! "+vars")
(load! "+repl")
(load! "+funcs")
(load! "+advice")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(use-package! abl-mode        :defer t)
(use-package! agentspeak-mode :defer t)
(use-package! ceptre-mode     :defer t)
(use-package! instal-mode     :defer t)
(use-package! jacamo-mode     :defer t)
(use-package! versu-mode      :defer t)
(use-package! soar-mode       :defer t)
(use-package! clips-mode      :defer t)
(use-package! netlogo-mode    :defer t)


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
