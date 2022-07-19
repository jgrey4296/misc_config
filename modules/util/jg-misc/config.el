;;; config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+advice")
(load! "+registers")

(after! evil
  (load! "+bindings")
)
(after! epa
  ;; Ascii output of encryptions:
  (setq epa-armor t)
  )
(after! (evil evil-snipe)
  (push 'dired-mode evil-snipe-disabled-modes)
  )
(after! evil-quickscope
  ;; TODO (spacemacs/set-leader-keys "t q" '+jg-personal-toggle-quickscope-always)
  (global-evil-quickscope-always-mode 1)
  )
(after! (dired dired-quick-sort)
  (setq dired-quick-sort-group-directories-last ?y)
  )
(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  )

(use-package! free-keys
  :commands (free-keys free-keys-set-prefix)
  :config
  (evil-make-intercept-map free-keys-mode-map)
  )

(use-package! undo-tree
  :config
  ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; file savings (these files add up over time and zstd is so incredibly fast).
  (when (executable-find "zstd")
    (defadvice! doom--undo-tree-make-history-save-file-name-a (file)
      :filter-return #'undo-tree-make-history-save-file-name
      (concat file ".zst")))

  ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  (defadvice! doom--undo-tree-strip-text-properties-a (&rest _)
    :before #'undo-list-transfer-to-tree
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; in the echo-area.
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)

  )
(use-package! cedet)
(use-package! semantic
  :defer t
  :config
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'semantic-highlight-func-mode)
  (add-to-list 'semantic-new-buffer-setup-functions
               '(emacs-lisp-mode . semantic-default-elisp-setup))
  ;; TODO setup semantic more, add helm etc
  )
(use-package! evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :config
  (defun iedit-show-all()
    " Override iedit's show all so it doesn't mess with invisible line movement"
    (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
    (remove-overlays nil nil iedit-invisible-overlay-name t)
  )

)
(use-package! timeline-mode)
