;; Misc AI Languages:

(after! evil
  (load! "+bindings")
  )

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

(after! ob-ccalc
    (org-babel-do-load-languages
     'org-babel-load-languages '((ccalc . t)))
    (push '("ccalc" . prolog) org-src-lang-modes)
    (add-to-list 'auto-mode-alist '("\\.ccalc$" . prolog-mode))
    )
(after! ob-clingo
    (org-babel-do-load-languages
     'org-babel-load-languages '((clingo . t)))
    (push '("clingo" . prolog) org-src-lang-modes)
    (add-to-list 'auto-mode-alist '("\\.lp$" . prolog-mode))
  )
(after! ob-prolog
    (org-babel-do-load-languages
     'org-babel-load-languages '((prolog . t)))
    (setq org-babel-prolog-command "swipl")
    (add-to-list 'auto-mode-alist '("\\.lp$" . prolog-mode))
  )
(use-package-hook! proof-general :post-config
  (set-face-attribute 'proof-locked-face nil
                      :inverse-video t
                      :underline nil
                      )
)
