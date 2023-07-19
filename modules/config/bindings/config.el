;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+vars")

(after! (evil faster-whichkey)
  (load! "+leader-bindings")
  (load! "submaps/+evil-bindings")
  (load! "+misc-bindings")
  (provide 'jg-bindings-total)
  (message "Core JG Bindings Set")
  )

(use-package! faster-whichkey
  :after (which-key general)
  :config
  (faster-whichkey-toggle)
  )


;; (use-package! which-key
;;   :hook (doom-first-input . which-key-mode)
;;   :init
;;   (setq which-key-sort-order #'which-key-key-order-alpha
;;         which-key-sort-uppercase-first nil
;;         which-key-add-column-padding 1
;;         which-key-max-display-columns nil
;;         which-key-min-display-lines 6
;;         which-key-side-window-slot -10)
;;   :config
;;   (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
;;   (add-hook! 'doom-before-reload-hook
;;     (defun doom-reset-which-key-replacements-h ()
;;       (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))
;;   ;; general improvements to which-key readability
;;   (which-key-setup-side-window-bottom)
;;   (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

;;   (which-key-add-key-based-replacements doom-leader-key "<leader>")
;;   (which-key-add-key-based-replacements doom-localleader-key "<localleader>")
;; )
