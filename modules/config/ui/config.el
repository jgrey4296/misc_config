;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "utils/+state-hl-lines")
(load! "utils/+faces")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(add-hook! 'doom-init-ui-hook  'rainbow-delimiters-mode)
(add-hook! 'doom-init-ui-hook

(defun +jg-ui-load-advice () (load! "utils/+advice")))

(use-package! hl-line
  :defer t
  :init
  (global-hl-line-mode)
  )

(use-package! hi-lock
  :defer t
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )

(use-package! auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :init

(defvar auto-highlight-symbol-mode nil)
  )

(use-package! whitespace
  :commands whitespace-mode
  :init

(defvar whitespace-mode nil)
  )

(use-package! centered-cursor-mode
  :commands centered-cursor-mode
  :init

(defvar centered-cursor-mode nil)
  )

(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )

(use-package! palette-mode
  :mode ("\\.palette" . palette-mode)
  :commands palette-mode
  )

(use-package! evil-visual-mark-mode :defer t)

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

(use-package! anzu
  :after-call isearch-mode)

(use-package! evil-anzu
  :when (modulep! :editor evil)
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

(spec-handling-new! modeline global-mode-string nil collect
                    ;; formatted as mode-line-format specifies
                    val
                    )
