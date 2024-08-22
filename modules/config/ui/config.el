;;; config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(local-load! "+tree-views")
(local-load! "+highlighting")
(local-load! "+colours")
(local-load! "+modeline")

(advice-remove 'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)
(advice-add 'kill-current-buffer           :before-until #'+jg-ui-kill-buffer-override)
(advice-add 'doom-modeline-propertize-icon :around #'+modeline-disable-icon-in-daemon-a)
(advice-add 'ws-butler-after-save          :around #'+modeline--inhibit-modification-hooks-a)

(add-hook! 'doom-first-file-hook #'transient-toggles-minor-mode)

;;-- whitespace

(use-package! whitespace
  :commands whitespace-mode
  :init
  (defvar whitespace-mode nil)

  )
;;-- end whitespace

;;-- search results

(use-package! anzu
  :after-call isearch-mode
  )

;;-- end search results

;;-- transient

(use-package! transient)

(use-package! transient-macros
  :after transient
  :config
  (+jg-ui-build-main-toggle-transient)
  ;; Assemble from the parts
  (transient-append-suffix 'jg-toggle-main
    '(1 0)
    [;; subgroups
     jg-toggle-debugs-transient
     jg-toggle-guides-transient
     jg-toggle-nav-transient
     jg-toggle-visuals-transient
     jg-toggle-wrap-transient

     ]
    )
  (transient-append-suffix 'jg-toggle-main
    '(2 0 -1)
    '(transient-macro-call-debug-on-error)
    )
  (provide 'transient-toggles)
  )

;;-- end transient

(use-package! fringe)
