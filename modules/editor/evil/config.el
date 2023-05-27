;;; editor/evil/config.el -*- lexical-binding: t; -*-

(load! "+defs")
(load! "+vars")
(load! "+spec-defs")
(after! evil-ex (load! "+commands") (load! "+evil-ex-setup"))

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

  ;; PERF: Stop copying the selection to the clipboard each time the cursor
  ;; moves in visual mode. Why? Because on most non-X systems (and in terminals
  ;; with clipboard plugins like xclip.el active), Emacs will spin up a new
  ;; process to communicate with the clipboard for each movement. On Windows,
  ;; older versions of macOS (pre-vfork), and Waylang (without pgtk), this is
  ;; super expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; UX: It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  (add-hook! '(doom-load-theme-hook doom-after-modules-config-hook)
    (defun +evil-update-cursor-color-h ()
      " Change the cursor color in emacs state. We do it this roundabout way
        to ensure changes in theme doesn't break these colors. "
      (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
      (put 'cursor 'evil-normal-color (face-background 'cursor)))
    )

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police this itself, so we must.
  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)

  (add-hook! 'doom-escape-hook
    (defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t))
    )

  ;; --- evil hacks -------------------------
  (after! eldoc
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state 'evil-insert 'evil-change 'evil-delete 'evil-replace))

  (unless noninteractive
    (add-hook! 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (doom-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size))))
    )

  (defadvice! +evil--dont-move-cursor-a (fn &rest args)
    " HACK '=' moves the cursor to the beginning of selection. Disable this,
      since it's more disruptive than helpful. "
    :around #'evil-indent
    (save-excursion (apply fn args))
    )

  (defadvice! +evil--make-numbered-markers-global-a (char)
    " REVIEW In evil, registers 2-9 are buffer-local. In vim, they're global,
    so... Perhaps this should be PRed upstream? "
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  (defadvice! +evil--fix-local-vars-a (&rest _)
    " REVIEW Fix #2493: dir-locals cannot target fundamental-mode when evil-mode
    is active. See hlissner/doom-emacs#2493. Revert this if
    emacs-evil/evil#1268 is resolved upstream. "
    :before #'turn-on-evil-mode
    (when (eq major-mode 'fundamental-mode)
      (hack-local-variables)))

  (defadvice! +evil--fix-helpful-key-in-evil-ex-a (key-sequence)
    " HACK Invoking helpful from evil-ex throws a 'No recursive edit is in
        progress' error because, between evil-ex and helpful,
       `abort-recursive-edit' gets called one time too many. "
    :before #'helpful-key
    (when (evil-ex-p)
      (run-at-time 0.1 nil #'helpful-key key-sequence)
      (abort-recursive-edit)))

  ;; Make J (evil-join) remove comment delimiters when joining lines.
  (advice-add #'evil-join :around #'+evil-join-a)

  (defadvice! +evil--no-squeeze-on-fill-a (fn &rest args)
    " Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
        spaces. It doesn't in vim, so it shouldn't in evil. "
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply fn args)))

  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil-replace-filename-modifiers-a)

  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil-window-split-a)
  (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit-a)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments' to disable)
  (advice-add #'evil-open-above :around #'+evil--insert-newline-above-and-respect-comments-a)
  (advice-add #'evil-open-below :around #'+evil--insert-newline-below-and-respect-comments-a)

  ;; --- custom interactive codes -----------
  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type regexp-match
    :runner (lambda (flag &optional arg) (+evil-ex-regexp-match flag arg 'inverted)))
  (evil-ex-define-argument-type regexp-global-match
    :runner +evil-ex-regexp-match)

  (defun +evil--regexp-match-args (arg)
    (when (evil-ex-p)
      (cl-destructuring-bind (&optional arg flags)
          (evil-delimited-arguments arg 2)
        (list arg (string-to-list flags)))))

  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg regexp-match
    (+evil--regexp-match-args evil-ex-argument))

  (evil-define-interactive-code "<//!>"
    :ex-arg regexp-global-match
    (+evil--regexp-match-args evil-ex-argument))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties '+evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties '+evil:align-right :ex-arg 'regexp-match)
  (evil-add-command-properties '+multiple-cursors:evil-mc :ex-arg 'regexp-global-match)

  ;; Lazy load evil ex commands
  (delq! 'evil-ex features)
  (add-transient-hook! 'evil-ex (provide 'evil-ex))
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
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible))

(use-package! evil-embrace
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (LaTeX-mode      . embrace-LaTeX-mode-hook)
  :hook (LaTeX-mode      . +evil-embrace-latex-mode-hook-h)
  :hook (org-mode        . embrace-org-mode-hook)
  :hook (ruby-mode       . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((c++-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode) . +evil-embrace-angle-bracket-modes-hook-h)
  :init
  (after! evil-surround (evil-embrace-enable-evil-surround-integration))
  :config
  (spec-handling-new-hooks! evil-embrace
                            ;; TODO
                            (setq embrace--pairs-list (append val embrace--pairs-list))
                            )

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
    (embrace-add-pair ?> "<" ">"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]"))
  )

(use-package! evil-escape
  :commands evil-escape
  :hook (doom-first-input . evil-escape-mode)
  :init
  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook! 'evil-escape-inhibit-functions
    (defun +evil-inhibit-escape-in-minibuffer-fn ()
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p)))))
  )

(use-package! evil-exchange
  :commands evil-exchange
  :config
  (add-hook! 'doom-escape-hook
    (defun +evil--escape-exchange-h ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t)))
  )

(use-package! evil-quick-diff
  :commands (evil-quick-diff evil-quick-diff-cancel))

(use-package! evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

(use-package! evil-snipe
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :hook (doom-first-input . evil-snipe-override-mode)
  :hook (doom-first-input . evil-snipe-mode)
  :init
  (map! :after evil-snipe
        :map evil-snipe-mode-map
        :nm "S" nil
        :nm "s" nil
        )
)

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
  :hook (doom-first-file . global-evil-quickscope-mode)
  )

(use-package! evil-escape
  :after evil)

(use-package! evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :config

(define-advice iedit-show-all (:override ()
                                 +jg-misc-iedit-show-all)
    " Override iedit's show all so it doesn't mess with invisible line movement"
    (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
    (remove-overlays nil nil iedit-invisible-overlay-name t)
  )

)

(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! evil-visual-mark-mode :defer t)

(use-package! evil-anzu
  :when (modulep! :editor evil)
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1)
  )

(use-package! exato
  :commands evil-outer-xml-attr evil-inner-xml-attr
  )
