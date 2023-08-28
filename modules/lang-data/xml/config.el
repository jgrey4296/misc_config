;;; config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! (jg-bindings-total jg-dired) "+bindings")

(use-package! mhtml-mode
  :commands mhtml-mode
  :config
  (add-hook! (mhtml-mode-hook html-mode-hook)
             #'tree-sitter!
             )
  )

(use-package! nxml-mode
  :commands nxml-mode
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  (setq-hook! 'nxml-mode-hook tab-width nxml-child-indent)
  (add-hook! 'nxml-mode-hook 'hs-minor-mode)
  )

;;; config.el ends here
