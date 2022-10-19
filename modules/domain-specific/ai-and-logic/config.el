;; Misc AI Languages:

(after! (coq jg-bindings-total)
  (load! "+bindings")
  )

(load! "+indent")
(load! "+vars")
(load! "+repl")
(load! "+funcs")
(load! "+advice")
(use-package! abl-mode)
(use-package! agentspeak-mode)
(use-package! ceptre-mode)
(use-package! instal-mode)
(use-package! jacamo-mode)
(use-package! versu-mode)
(use-package! soar-mode)
(use-package! clips-mode)
(use-package! netlogo-mode
  :defer t
  :commands (netlogo-mode)
  )


(after! org
  (push '("clingo" . prolog) org-src-lang-modes)
  (push '("ccalc" . prolog) org-src-lang-modes)
  )

(after! ob-prolog
  (setq org-babel-prolog-command "swipl")
  (add-to-list 'auto-mode-alist '("\\.lp$" . prolog-mode))
  )

(use-package-hook! proof-general :post-config
  (set-face-attribute 'proof-locked-face nil
                      :inverse-video t
                      :underline nil
                      )
)

(use-package! pasp-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.lp4" . pasp-mode ))
  (add-hook 'pasp-mode-hook (lambda ()
                              (setq-local indent-line-function '+jg-logic-pasp-indent)))

  )
