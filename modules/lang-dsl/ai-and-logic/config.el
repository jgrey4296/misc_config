;; Misc AI Languages:

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(use-package! agentspeak-mode        :defer t)

(use-package! instal-mode            :defer t)

(use-package! jacamo-mode            :defer t)

(use-package! soar-mode              :defer t)

(use-package! clips-mode             :defer t)

(use-package! ob-prolog
  :defer t
  :init
  (setq org-babel-prolog-command "swipl")
  )

(use-package! ob-clingo
  :defer t
  )

(use-package! ob-instal
  :defer t
  )

(use-package! pasp-mode
  :commands pasp-mode
  :after evil
  :init
  (setq-hook! 'pasp-mode-hook
    indent-line-function '+jg-logic-pasp-indent
    )
  (advice-add 'pasp-generate-command   :around #'+jg-pasp-generate-args)
  (advice-add 'pasp-run-clingo         :override #'+jg-pasp-run-clingo)
  (advice-add 'pasp-compilation-filter :override #'+jg-pasp-compilation-filter)
  )

(use-package! prolog
  :init
  (add-hook! 'prolog-mode-hook
             #'librarian-insert-minor-mode
             #'outline-minor-mode)

  (setq-hook! 'prolog-mode-hook
    outline-regexp (rx (+? nonl) ":-")
    outline-heading-end-regexp (rx line-end)
    outline-level #'(lambda () 1)
    )
  )
