;;; checkers/spell/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! ispell
  :preface
  (defer-feature! ispell ispell-minor-mode)
  :config
  (add-hook! 'text-mode-hook #'+spell-remove-run-together-switch-for-aspell-h)

  (setq ispell-aspell-dict-dir     (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir     (ispell-get-aspell-config-value "data-dir")
        )
  )

(use-package! spell-fu
  :unless (modulep! +flyspell)
  :when (executable-find "aspell")
  :hook (text-mode . spell-fu-mode)
  :general ([remap ispell-word] #'+spell/correct)
  :preface

  (defvar +spell-correct-interface #'+spell-correct-ivy-fn)

  :init
  (defvar +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face markdown-html-attr-name-face markdown-html-attr-value-face markdown-html-tag-name-face markdown-inline-code-face markdown-link-face markdown-markup-face markdown-plain-url-face markdown-reference-face markdown-url-face))
      (org-mode
       . (org-block org-block-begin-line org-block-end-line org-cite org-cite-key org-code org-date org-footnote org-formula org-inline-src-block org-latex-and-related org-link org-meta-line org-property-value org-ref-cite-face org-special-keyword org-tag org-todo org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-kill org-todo-keyword-outd org-todo-keyword-todo org-todo-keyword-wait org-verbatim))
      (latex-mode
       . (font-latex-math-face font-latex-sedate-face font-lock-function-name-face font-lock-keyword-face font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck.")
  (add-hook! '(yaml-mode-hook conf-mode-hook prog-mode-hook) #'spell-fu-mode)
  :config

  (add-hook! 'spell-fu-mode-hook #'+spell-init-excluded-faces-h)

  )

(use-package! flyspell ; built-in
  :defer t
  :preface
  ;; `flyspell' is loaded at startup. In order to lazy load its config we need to pretend it isn't loaded.

  (defer-feature! flyspell flyspell-mode flyspell-prog-mode)
  :init
  (add-hook! '(org-mode-hook
               markdown-mode-hook
               TeX-mode-hook
               rst-mode-hook
               mu4e-compose-mode-hook
               message-mode-hook
               git-commit-mode-hook)
             #'flyspell-mode)

  (when (modulep! +everywhere)
    (add-hook! '(yaml-mode-hook
                 conf-mode-hook
                 prog-mode-hook)
               #'flyspell-prog-mode))

  :config
  (provide 'ispell) ; forcibly load ispell configs

  (setq flyspell-issue-welcome-flag nil
        ;; Significantly speeds up flyspell, which would otherwise print
        ;; messages for every word when checking the entire buffer
        flyspell-issue-message-flag nil)

  (add-hook! 'flyspell-mode-hook #'+spell-inhibit-duplicate-detection-maybe-h)

  ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
  ;; used in their respective major modes.
  (add-hook 'flyspell-mode-hook #'+spell-init-flyspell-predicate-h)

  (let ((flyspell-correct
         (cmds! (and (not mark-active)
                     (not (and (bound-and-true-p evil-local-mode)
                               (or (evil-insert-state-p)
                                   (evil-emacs-state-p))))
                     (memq 'flyspell-incorrect (face-at-point nil t)))
                #'flyspell-correct-at-point)))
    (map! :map flyspell-mouse-map
          "RET"    flyspell-correct
          [return] flyspell-correct
          [mouse-1] #'flyspell-correct-at-point))
  )

(use-package! flyspell-correct
  :commands flyspell-correct-previous
  :config
  (require 'flyspell-correct-ivy nil t)
  )

(use-package! flyspell-lazy
  :after flyspell
  :config
  ;; Fix #3357: flyspell-lazy inhibits flyspell entirely in message-mode
  ;; derivatives (e.g. for notmuch users).
  (setq-hook! 'message-mode-hook flyspell-lazy-disallow-buffers nil)
  ;; (flyspell-lazy-mode +1)
  )
