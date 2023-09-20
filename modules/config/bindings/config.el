;;; config/default/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! (evil faster-whichkey) "+bindings" "+misc-bindings") ;; -> jg-bindings-core

(after! (jg-evil-bindings which-key) (provide 'jg-bindings-total))

(use-package! faster-whichkey
  :after (general)
  :config
  (faster-whichkey-toggle)
  (setq which-key-replacement-alist nil)
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re)) nil . "maximize") which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) d\\'" prefix-re)) nil . "doom") which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) r\\'" prefix-re)) nil . "reload") which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) b\\'" prefix-re)) nil . "bindings") which-key-replacement-alist)
    )
)

(use-package! which-key)
(use-package! general)

;;(use-package! which-key
;;  :hook (doom-first-input . which-key-mode)
;;  :init
;;  (setq which-key-sort-order #'which-key-key-order-alpha
;;        which-key-sort-uppercase-first nil
;;        which-key-add-column-padding 1
;;        which-key-max-display-columns nil
;;        which-key-min-display-lines 6
;;        which-key-side-window-slot -10)
;;  :config
;;  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
;;  (add-hook! 'doom-before-reload-hook
;;    (defun doom-reset-which-key-replacements-h ()
;;      (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))
;;  ;; general improvements to which-key readability
;;  (which-key-setup-side-window-bottom)
;;  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

;;  (which-key-add-key-based-replacements doom-leader-key "<leader>")
;;  (which-key-add-key-based-replacements doom-localleader-key "<localleader>")
;;  )
