;; Misc AI Languages:


(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! agentspeak-mode :defer t)
(use-package! instal-mode     :defer t)
(use-package! jacamo-mode     :defer t)
(use-package! soar-mode       :defer t)
(use-package! clips-mode      :defer t)

(after! org
  (push '("clingo" . prolog) org-src-lang-modes)
  (push '("ccalc" . prolog) org-src-lang-modes)
  )

(after! ob-prolog
  (setq org-babel-prolog-command "swipl")
  )

(use-package! pasp-mode
  :after evil
  :init
  (add-hook 'pasp-mode-hook (lambda ()
                              (setq-local indent-line-function '+jg-logic-pasp-indent)))

  )
