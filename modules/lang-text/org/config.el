;;; lang/org/config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(local-load! "+definitions")
(local-load! "+spec-defs")
(local-load! "+vars")
(local-load! "+extra-configs")
(local-load! "+export")
(local-load! "+babel")
(defer-load! jg-bindings-core "+bindings" "+agenda-bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(use-package! org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  (provide 'smartparens-org)
  ;; Custom org modules
  ;; (dolist (flag (doom-module-context-get :flags))
  ;;   (load! (concat "contrib/" (substring (symbol-name flag) 1)) nil t))

  ;; (dolist (flag (doom-module :lang 'org :flags))
  ;;   (load! (concat "contrib/" (substring (symbol-name flag) 1)) nil t))

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not byte-compile-current-file))
    (unless (doom-context-p 'reload)
      (message "`org' was already loaded by the time lang/org loaded, this may cause issues"))
    (provide 'org)
    )

  :config
  (add-to-list 'doom-debug-variables 'org-export-async-debug)

  (+org-init-appearance-h)
  (local-load! "+tags")

  ;; Don't number headings with these tags
  (setq org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
        org-num-skip-tags '("noexport" "nonum"))

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)

  ;;-- advice
  ;; General
  ;; HACK For functions that dodge `org-open-at-point-functions', like
  ;;   `org-id-open', `org-goto', or roam: links.
  (advice-add 'org-mark-ring-push                   :around #'doom-set-jump-a)
  (advice-add 'org-insert-heading                   :after #'evil-insert)
  (advice-add 'server-visit-files                   :around #'+org--server-visit-files-a)
  (advice-add 'org-id-locations-save                :before-while #'+org--fail-gracefully-a)
  (advice-add 'org-id-locations-load                :before-while #'+org--fail-gracefully-a)
  (advice-add 'org-return                           :after #'+org-fix-newline-and-indent-in-src-blocks-a)
  (advice-add 'org-eldoc-documentation-function     :before-until #'doom-docs--display-docs-link-in-eldoc-a)
  ;; (advice-add #'org-insert-subheading            :after #'evil-insert)
  ;; Open help:* links with helpful-* instead of describe-*
  (advice-add 'org-link--open-help              :around #'doom-use-helpful-a)
  (advice-add 'org-mks                          :around #'+org--remove-customize-option-a)
  (advice-add 'org-id-open                      :around #'+org--follow-search-string-a)
  (advice-add 'org-persist-write:index          :before #'+org--recursive-org-persist-mkdir-a)
  (advice-add 'org-cycle-set-startup-visibility :before-until #'+org--more-startup-folded-options-a)
  (advice-add 'org-fix-tags-on-the-fly          :before-while #'+org--respect-org-auto-align-tags-a)
  (advice-add 'org-footnote-action              :after #'+org--recenter-after-follow-link-a)
  (advice-add 'org-follow-timestamp-link        :after #'+org--recenter-after-follow-link-a)
  (advice-add 'org-link-open-as-file            :after #'+org--recenter-after-follow-link-a)
  (advice-add 'org-link-search                  :after #'+org--recenter-after-follow-link-a)
  (advice-add 'org-format-outline-path :around #'+org--strip-properties-from-outline-a)
  (advice-add 'org-get-agenda-file-buffer :around #'+org--optimize-backgrounded-agenda-buffers-a)
  (advice-add 'org-display-inline-images :around #'+org--fix-inline-images-for-imagemagick-users-a)
  (advice-add 'org-id-new :filter-return #'+org--fix-inconsistent-uuidgen-case-a)

  ;; ui
  (advice-add 'evil-org-open-below                  :around #'+org-fix-window-excursions-a)
  (advice-add 'evil-org-open-above                  :around #'+org-fix-window-excursions-a)
  (advice-add 'org-indent-region                    :around #'+org-fix-window-excursions-a)
  (advice-add 'org-indent-line                      :around #'+org-fix-window-excursions-a)
  (advice-add 'toc-org-insert-toc                   :around #'+org-inhibit-scrolling-a)

  ;; Disable doom docs org
  (fset 'doom-docs-mode #'ignore)
  (fset 'doom-docs-org-mode #'ignore)

  ;;-- end advice

  ;;-- hooks
  (add-hook 'org-open-at-point-functions #'doom-set-jump-h)

  ;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  (defun jg-turn-off-smartparens-org ()
    (smartparens-mode -1)
    )

  ;; Add our general hooks after the submodules, so that any hooks the
  ;; submodules add run after them, and can overwrite any defaults if necessary.
  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays
             #'doom-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace'; shows a lot of false positives
             #'doom-disable-show-trailing-whitespace-h
             #'+org-enable-auto-reformat-tables-h
             ;; #'+org-enable-auto-update-cookies-h
             #'org-indent-mode
             #'abbrev-mode
             #'org-set-regexps-and-options
             #'+jg-org-startup-agenda-h
             #'+jg-org-startup-reference-h
             #'+jg-org-startup-package-h
             #'librarian-insert-minor-mode
             #'jg-turn-off-smartparens-org
             )
  (setq-hook! 'org-mode-hook
    tab-width 8
    org-todo-keywords      jg-org-todo-keywords
    org-todo-keyword-faces jg-org-todo-faces
    org-refile-targets     jg-org-refile-targets
    )

  ;;-- end hooks

  (setq-default  org-todo-keywords jg-org-todo-keywords
                 org-todo-keyword-faces jg-org-todo-faces
                 org-refile-targets jg-org-refile-targets
                 )

  (when (modulep! :lang markdown) (add-to-list 'org-export-backends 'md))
  (org-element-update-syntax)
  )

(use-package! org-clock ; built-in
  :commands org-clock-save
  :init

  (advice-add 'org-clock-in      :before #'+org--clock-load-a)
  (advice-add 'org-clock-out     :before #'+org--clock-load-a)
  (advice-add 'org-clock-in-last :before #'+org--clock-load-a)
  (advice-add 'org-clock-goto    :before #'+org--clock-load-a)
  (advice-add 'org-clock-cancel  :before #'+org--clock-load-a)

  :config
  (setq org-clock-persist 'history
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t
        ;; Remove log if task was clocked for 0:00 (accidental clocking)
        org-clock-out-remove-zero-time-clocks t
        ;; The default value (5) is too conservative.
        org-clock-history-length 20)

  ;; Hooks
  (add-hook! 'org-clock-cancel-hook

             )
  (add-hook! 'org-clock-in-hook

             )
  (add-hook! 'org-clock-in-prepare-hook

             )
  (add-hook! 'org-clock-before-select-task-hook

             )
  (add-hook! 'org-clock-out-hook

             )
  (add-hook! 'org-clock-goto-hook

             )

  (add-hook! 'kill-emacs-hook
             #'org-clock-save
             )

  )

(use-package! evil-org
  :when (modulep! :editor evil)
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :hook (doom-docs-org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             #'+org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'+org-clear-babel-results-h)
)

(use-package! link-hint
  :config
  ;; override default org link to open externally sometimes
  (link-hint-define-type 'org-link
    :next #'link-hint--next-org-link
    :at-point-p #'link-hint--org-link-at-point-p
    :vars '(org-mode org-agenda-mode org-link-minor-mode)
    :open #'+jg-org-link-hint-external
    :open-multiple t
    :copy #'kill-new)
  (push 'org-link link-hint-types)
  )

(use-package! org-journal
  :after org
  :init
  ;; HACK `org-journal' adds a `magic-mode-alist' entry for detecting journal
  ;;      files, but this causes us lazy loaders a big problem: an unacceptable
  ;;      delay on the first file the user opens, because calling the autoloaded
  ;;      `org-journal-is-journal' pulls all of `org' with it. So, we replace it
  ;;      with our own, extra layer of heuristics.
  (add-to-list 'magic-mode-alist '(+org-journal-p . org-journal-mode))

  :config
  ;; Remove the orginal journal file detector and rely on `+org-journal-p'
  ;; instead, to avoid loading org-journal until the last possible moment.
  (setq magic-mode-alist (assq-delete-all 'org-journal-is-journal magic-mode-alist))

  ;; Setup carryover to include all configured TODO states. We cannot carry over
  ;; [ ] keywords because `org-journal-carryover-items's syntax cannot correctly
  ;; interpret it as anything other than a date.
  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"PROJ\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"HOLD\"")

)

(use-package! org-capture
  :defer t
  :config
  (advice-add 'org-capture-expand-file          :filter-args #'+org--capture-expand-variable-file-a)
  (advice-add 'org-capture-refile               :after #'+org-capture-refile-cleanup-frame-a)

  ;; Hooks
  (add-hook! 'org-capture-prepare-finalize-hook

             )
  (add-hook! 'org-capture-before-finalize-hook

             )
  (add-hook! 'org-capture-mode-hook
             #'+org-show-target-in-capture-header-h
             )

  (add-hook! 'org-capture-after-finalize-hook
             #'+org-capture-cleanup-frame-h
             )

  )

(use-package! org-agenda
  :defer t
  :config

  (evil-set-initial-state 'org-agenda-mode 'normal)
  ;;hooks
  (add-hook! 'org-agenda-entry-text-cleanup-hook

             )
  (add-hook! 'org-agenda-mode-hook
             #'+org-habit-resize-graph-h
             )
  (add-hook! 'org-agenda-before-write-hook

             )
  (add-hook! 'org-agenda-after-show-hook

             )
  (add-hook! 'org-agenda-cleanup-fancy-diary-hook

             )
  (add-hook! 'org-agenda-filter-hook

             )
  (add-hook! 'org-agenda-finalize-hook
             #'+org-exclude-agenda-buffers-from-workspace-h
             #'+org-defer-mode-in-agenda-buffers-h
             )

  )
