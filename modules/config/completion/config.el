;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
(load! "+vars")
(after! (evil jg-bindings-total)
  (load! "+bindings")
)
(after! (ivy counsel)
  (load! "+ivys")
  )

;;-- helm
(use-package! helm
  :commands helm
  :config
  (setq helm-completing-read-handlers-alist nil)
  (load! "+helms")
  )
(use-package! helm-files :defer t)
(use-package! helm-gtags :defer t)

;;-- end helm

;;-- company
(after! (company minibuffer)
  (add-hook! 'minibuffer-inactive-mode-hook :append #'company-mode)
  (add-hook! 'minibuffer-setup-hook :append #'company-mode)
  )
(after! (company gtags helm-gtags python)
  (set-company-backend! 'python-mode 'company-gtags)
  )

;;-- end company
