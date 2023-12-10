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
  :commands pasp-mode
  :after evil
  :init
  (setq-hook! 'pasp-mode-hook
    indent-line-function '+jg-logic-pasp-indent
    )
  )

(use-package! prolog
  :init
  (add-hook! 'prolog-mode-hook
             #'general-insert-minor-mode)
  )
