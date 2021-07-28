;; Misc AI Languages:
(use-package! abl-mode)
(use-package! agentspeak-mode)
(use-package! ceptre-mode)
(use-package! instal-mode)
(use-package! jacamo-mode)
(use-package! versu-mode)
(use-package! soar-mode)
(use-package! clips-mode
  :init
  (setq inferior-clips-program "clips")
  )
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
(after! proof-general
    (setq proof-splash-enable nil
          proof-three-window-enable nil
          coq-compile-before-require t
          coq-accept-proof-using-suggestion 'never
          )
    (push 'coq-mode +jg-personal-major-modes)
    (evil-define-key 'normal proof-mode-map
      (kbd "RET") 'proof-goto-point
      (kbd "DEL") 'proof-undo-last-successful-command
      (kbd "<down>") 'proof-assert-next-command-interactive
      (kbd "<up>") 'proof-undo-last-successful-command
      )

    (spacemacs|use-package-add-hook proof-general
      :post-config
      (set-face-attribute 'proof-locked-face nil
                          :inverse-video t
                          :underline nil
                          )
      )

    ;; (spacemacs/set-leader-keys-for-major-mode 'coq-mode
    ;;   (kbd "i m") 'coq-insert-match
    ;;   (kbd "i i") 'coq-insert-intros
    ;;   (kbd "i t t") 'coq-insert-tactic
    ;;   (kbd "i t s") 'coq-insert-solve-tactic
    ;;   )
  )
