;;; editor/evil/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! (evil-collection evil-ex) "+evil-ex-setup")

(defer-load! jg-bindings-core "+bindings") ;; -> jg-evil-bindings

(use-package! evil
  :hook (doom-after-modules-config . evil-mode)
  :demand t
  :preface
  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
    evil-ex-hl-update-delay 0.25)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties '+evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties '+evil:align-right :ex-arg 'regexp-match)
  (evil-add-command-properties '+multiple-cursors:evil-mc :ex-arg 'regexp-global-match)

    ;; Allow eldoc to trigger directly after changing states
  (after! eldoc (eldoc-add-command 'evil-normal-state 'evil-insert 'evil-change 'evil-delete 'evil-replace))
  (unless noninteractive (add-hook! 'after-save-hook #'+evil-display-vimlike-save-message-h))

  ;; PERF: Stop copying the selection to the clipboard each time the cursor
  ;; moves in visual mode. Why? Because on most non-X systems (and in terminals
  ;; with clipboard plugins like xclip.el active), Emacs will spin up a new
  ;; process to communicate with the clipboard for each movement. On Windows,
  ;; older versions of macOS (pre-vfork), and Waylang (without pgtk), this is
  ;; super expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; UX: It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)

  ;;-- advice
  (advice-add 'help-with-tutorial   :after (lambda (&rest _) (evil-emacs-state +1)))
  (advice-add 'evil-indent          :around #'+evil--dont-move-cursor-a)
  (advice-add 'evil-global-marker-p :after-until #'+evil--make-numbered-markers-global-a)
  (advice-add 'turn-on-evil-mode    :before #'+evil--fix-local-vars-a)
  (advice-add 'helpful-key          :before #'+evil--fix-helpful-key-in-evil-ex-a)

  ;; Make J (evil-join) remove comment delimiters when joining lines.
  (advice-add 'evil-join          :around #'+evil-join-a)
  (advice-add 'evil-fill          :around #'+evil--no-squeeze-on-fill-a)
  (advice-add 'evil-fill-and-move :around #'+evil--no-squeeze-on-fill-a)
  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add 'evil-ex-replace-special-filenames :override #'+evil-replace-filename-modifiers-a)
  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)
  ;; Focus and recenter new splits
  (advice-add 'evil-window-split  :override #'+evil-window-split-a)
  (advice-add 'evil-window-vsplit :override #'+evil-window-vsplit-a)
  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments' to disable)
  (advice-add 'evil-open-above              :around #'+evil--insert-newline-above-and-respect-comments-a)
  (advice-add 'evil-open-below              :around #'+evil--insert-newline-below-and-respect-comments-a)
  (advice-add 'iedit-show-all               :override #'+jg-evil-iedit-show-all)
  (advice-add 'counsel-mark--get-candidates :filter-args #'+jg-evil-marks-cleanup)
  (advice-add 'evil-find-char               :override #'+jg-evil-find-char)

  ;;-- end advice

  ;;-- hooks
  (add-hook 'doom-load-theme-hook           #'+evil-update-cursor-color-h)
  (add-hook 'doom-after-modules-config-hook #'+evil-update-cursor-color-h)
  (add-hook 'evil-insert-state-entry-hook   #'delete-selection-mode)
  (add-hook 'evil-insert-state-exit-hook    #'+default-disable-delete-selection-mode-h)
  ;; (add-hook 'evil-local-mode-hook           #'+jg-evil--auto-marks-h)

  ;;-- end hooks

  ;; Lazy load evil ex commands
  ;; (delq! 'evil-ex features)
  ;; (add-transient-hook! 'evil-ex (provide 'evil-ex))
  )

(use-package! evil-easymotion
  :after-call doom-first-input-hook
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil)))

  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible)
  )

(use-package! evil-embrace
  :defer t
  :hook (LaTeX-mode      . embrace-LaTeX-mode-hook)
  :hook (org-mode        . embrace-org-mode-hook)
  :hook (ruby-mode       . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :config
  (after! evil-surround (evil-embrace-enable-evil-surround-integration))
  (speckler-new-hook! evil-embrace (key val)
    "Construct evil embrace pairs lists"
    (setq embrace--pairs-list (append val embrace--pairs-list))
    )

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]"))
  )

(use-package! evil-escape
  :hook (doom-first-input . evil-escape-mode)
  :config
  (evil-escape-add-default-inhibitors)
  )

(use-package! evil-exchange
  :commands evil-exchange
  )

(use-package! evil-quick-diff
  :commands (evil-quick-diff evil-quick-diff-cancel)
  )

(use-package! evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

(use-package! evil-snipe :defer t)

(use-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package! evil-textobj-anyblock
  :defer t
)

(use-package! evil-traces
  :after evil-ex
  :config
  (pushnew! evil-traces-argument-type-alist
            '(+evil:align . evil-traces-global)
            '(+evil:align-right . evil-traces-global))
  (evil-traces-mode)
  )

(use-package! evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
)

(use-package! evil-quickscope
  :config
  (map! :map evil-quickscope-mode-map
        :nm "t" nil
        :nm "T" nil
        )
  )

(use-package! evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)

)

(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! evil-visual-mark-mode
  :defer t
  :hook (prog-mode . evil-visual-mark-mode)
  )

(use-package! evil-anzu
  :when (modulep! :editor evil)
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1)
  )

(use-package! evil-textobj-tree-sitter
  :after (evil tree-sitter which-key)
  :config
  (setq which-key-allow-multiple-replacements t)
  (pushnew! which-key-replacement-alist
            '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1")))
  )

(use-package! exato ;; xml attributes text objects
  :commands evil-outer-xml-attr evil-inner-xml-attr
  )

(use-package! evil-fringe-mark
  :defer t
  )
