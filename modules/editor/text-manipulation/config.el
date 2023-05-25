;;; util/text/config.el -*- lexical-binding: t; -*-
(load! "+defs")
(load! "+vars")
(load! "+spec-defs")
(after! jg-bindings-total
  (load! "evil/+operators")
  (load! "evil/+motions")
  (load! "evil/+state")
  (load! "evil/+text-obj")
  (load! "+bindings")
  (load! "+advice")
)
(load! "modes/+derived-modes")
(after! evil
  (remove-hook! 'after-change-major-mode-hook #'doom--setq-evil-shift-width-for-after-change-major-mode-h)
  )
(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode)
  )

(add-hook! 'doom-init-ui-hook :append #'+ligature-init-composition-table-h)

(use-package! academic-phrases :defer t)

(use-package! helm-wordnet :defer t)

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list)
)

(use-package! license-templates :defer t)

(use-package! lint-result-mode
  :config
  (add-hook 'lint-result-mode-hook '+fold/close-all)
  )

(use-package! vundo
  :commands vundo
  )

(use-package! undo-fu :defer t)

(use-package! timeline-mode :defer t)

(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :hook (doom-first-buffer . ws-butler-global-mode)
  )

(use-package! smartparens
  :hook (doom-first-buffer . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  (add-to-list 'doom-point-in-string-functions 'sp-point-in-string)
  (add-to-list 'doom-point-in-comment-functions 'sp-point-in-comment)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (require 'smartparens-config);; Load default smartparens rules for various languages

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook! 'eval-expression-minibuffer-setup-hook #'doom-init-smartparens-in-eval-expression-h)
  (add-hook! 'minibuffer-setup-hook #'doom-init-smartparens-in-minibuffer-maybe-h)

  ;; Smartparens breaks evil-mode's replace state
  (defvar doom-buffer-smartparens-mode nil)
  (add-hook! 'evil-replace-state-exit-hook #'doom-enable-smartparens-mode-maybe-h)
  (add-hook! 'evil-replace-state-entry-hook #'doom-disable-smartparens-mode-maybe-h)
)
