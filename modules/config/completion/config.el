;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;-- file loads
(load! "+vars")
(load! "+template-control")
(load! "+yasnippet")
(after! jg-bindings-total
  (load! "+bindings")
)
(after! (ivy counsel)
  (load! "+ivys")
  )
(after! 'helm
  (load! "+helms")
  )
;;-- end file loads

;;-- helm
(use-package! helm
  :commands helm
  :config
  (setq helm-completing-read-handlers-alist nil)
  )
(use-package! helm-files :defer t)
(use-package! helm-gtags :defer t)

;;-- end helm

;;-- company
(after! (company minibuffer)
  (add-hook! 'minibuffer-inactive-mode-hook :append #'company-mode)
  (add-hook! 'minibuffer-mode-hook :append #'company-mode)
  )
(after! (company gtags helm-gtags python)
  (set-company-backend! 'python-mode 'company-gtags)
  )

;;-- end company

;;-- hook setup
(defun +jg-completion-on-load-hook ()
  (require 'yasnippet)
  (advice-add '+snippet--completing-read-uuid :override #'+jg-completion-snippet--completing-read-uuid)
  (add-hook 'yas-prompt-functions #'+jg-completion-yas-prompt-fn -90)

  (after! doom-snippets
    (+jg-completion-activate-templates t)
    (yas-reload-all)
    )
  )
(add-hook 'doom-first-file-hook #'+jg-completion-on-load-hook)
(add-hook 'jg-reapply-hook '+jg-completion-file-interactive-activate)

;;-- end hook setup
