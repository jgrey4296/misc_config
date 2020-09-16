;;; specific/jg-logic-layer/config.el -*- lexical-binding: t; -*-

(defun jg-logic-layer/init-ob-ccalc ()
  (use-package ob-ccalc
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((ccalc . t)))
    (push '("ccalc" . prolog) org-src-lang-modes)
    (add-to-list 'auto-mode-alist '("\\.ccalc$" . prolog-mode))
    )
  )
(defun jg-logic-layer/init-ob-clingo ()
  (use-package ob-clingo
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((clingo . t)))
    (push '("clingo" . prolog) org-src-lang-modes)
    (add-to-list 'auto-mode-alist '("\\.lp$" . prolog-mode))
    )
  )
(defun jg-logic-layer/init-ob-prolog ()
  (use-package ob-prolog
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((prolog . t)))
    (setq org-babel-prolog-command "swipl")
    (add-to-list 'auto-mode-alist '("\\.lp$" . prolog-mode))
    )
  )

(defun jg-logic-layer/post-init-proof-general ()
    (setq proof-splash-enable nil
          proof-three-window-enable nil
          coq-compile-before-require t
          coq-accept-proof-using-suggestion 'never
          )
    (push 'coq-mode jg-spacemacs-main-layer/major-modes)
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
