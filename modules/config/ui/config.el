;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+spec-defs")
(after! jg-bindings-total
  (load! "+bindings")
  )

;;-- highlight

(use-package! hl-line
  :defer t
  :hook (doom-first-file . global-hl-line-mode)
  :config
  (after! evil
    ;; hooks for evil state entry hooks to change hl-line colour
    (add-hook! '(evil-normal-state-entry-hook
                 evil-insert-state-entry-hook
                 evil-visual-state-entry-hook
                 evil-motion-state-entry-hook
                 evil-emacs-state-entry-hook
                 evil-replace-state-entry-hook
                 evil-hybrid-state-entry-hook
                 evil-evilified-state-entry-hook
                 evil-lisp-state-entry-hook
                 evil-iedit-state-entry-hook)
               #'+jg-ui-state-line-change)
    )
  )

(use-package! hi-lock
  :defer t
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  (add-hook! '(pug-mode-hook haml-mode-hook) #'+hl-todo--use-face-detection-h)
)

(use-package! highlight-indent-guides
  :defer t
  :init
  (defvar highlight-indent-guides-mode nil)
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :config
  ;; (when (doom-context-p 'init)
  ;; call hligast late enough to be useful
  ;; (add-hook 'doom-first-buffer-hook #'highlight-indent-guides-auto-set-faces)
  ;; )

  ;; errors when first file is org: (add-hook! 'org-mode-local-vars-hook #'+indent-guides-disable-maybe-h)
  )

(use-package! highlight-parentheses
  :defer t
  :init
  (add-hook! doom-first-buffer
             #'global-highlight-parentheses-mode
             )
  )

(use-package! auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :init
  (defvar auto-highlight-symbol-mode nil)
  )

(use-package! hilit-chg
  :hook (doom-first-buffer . global-highlight-changes-mode)
  )

;;-- end highlight

;;-- whitespace
(use-package! whitespace
  :commands whitespace-mode
  :init
  (defvar whitespace-mode nil)
  )
;;-- end whitespace

;;-- cursor

(use-package! centered-cursor-mode
  :commands centered-cursor-mode
  :init
  (defvar centered-cursor-mode nil)
  )

;;-- end cursor

;;-- colours

(use-package! palette-mode
  :commands palette-mode
  )

(use-package! rainbow-mode
  :defer t
  :init
  (add-hook! 'prog-mode-hook 'rainbow-mode)
)

(use-package! rainbow-delimiters
  :config
  (add-hook! 'doom-init-ui-hook  #'rainbow-delimiters-mode)
  )

;;-- end colours

;;-- modeline

(use-package! all-the-icons
  :disabled t)

(use-package! doom-modeline
  :hook (doom-init-ui . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (defface doom-modeline-buffer-modified
    '((t (:inherit (error bold) :background unspecified)))
    "Face used for the \\='unsaved\\=' symbol in the mode-line."
    :group 'doom-modeline-faces)

  :config
  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (add-hook    'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook    'doom-load-theme-hook #'doom-modeline-refresh-bars)
  (add-hook!   'magit-mode-hook #'+modeline-hide-in-non-status-buffer-h)
  )

;;-- end modeline

;;-- search results

(use-package! anzu
  :after-call isearch-mode
  )

;;-- end search results

;;-- transient
;; (use-package! transient
;;   :config

;;   (transient-define-prefix jg-toggle-main ()
;;     "for accessing toggle settings"
;;     [

;;      ]
;;     [""
;;      ("q" "quit" transient-quit-one)
;;      ]
;;     )
;;   )

;;-- end transient
