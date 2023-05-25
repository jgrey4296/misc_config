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
  (defadvice! +hl-todo-clamp-font-lock-fontify-region-a (fn &rest args)
    "Fix an `args-out-of-range' error in some modes."
    :around #'hl-todo-mode
    (letf! #'font-lock-fontify-region (apply fn args))
    )

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

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook! 'org-mode-local-vars-hook #'+indent-guides-disable-maybe-h)
  )

(use-package! highlight-parentheses :defer t)

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
  (defvar whitespace-mode)
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
  :mode ("\\.palette" . palette-mode)
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

(use-package! doom-modeline
  :hook (doom-after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (when (>= emacs-major-version 29)
    ;; HACK: Emacs 29 treats `nil' for :background as invalid, and complains.
    ;;   `doom-modeline' hasn't updated its face to address this yet.
    ;; REVIEW: PR this fix to doom-modeline

(defface doom-modeline-buffer-modified
      '((t (:inherit (error bold) :background unspecified)))
      "Face used for the \\='unsaved\\=' symbol in the mode-line."
      :group 'doom-modeline-faces))

  :config
  ;; HACK Fix #4102 due to empty all-the-icons return value (caused by
  ;;      `doom--disable-all-the-icons-in-tty-a' advice) in tty daemon frames.

(defadvice! +modeline-disable-icon-in-daemon-a (fn &rest args)
    :around #'doom-modeline-propertize-icon
    (when (display-graphic-p)
      (apply fn args)))

  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS

(defvar mouse-wheel-down-event nil)

(defvar mouse-wheel-up-event nil)

  (add-hook    'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook    'doom-load-theme-hook #'doom-modeline-refresh-bars)
  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (add-hook! 'magit-mode-hook

(defun +modeline-hide-in-non-status-buffer-h ()
      "Show minimal modeline in magit-status buffer, no modeline elsewhere."
      (if (eq major-mode 'magit-status-mode)
          (doom-modeline-set-modeline 'magit)
        (hide-mode-line-mode))))

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so force them to behave.

(defadvice! +modeline--inhibit-modification-hooks-a (fn &rest args)
    :around #'ws-butler-after-save
    (with-silent-modifications (apply fn args)))
)

;;-- end modeline

;;-- search results

(use-package! anzu
  :after-call isearch-mode)

;;-- end search results
