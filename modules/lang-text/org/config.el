;;; lang/org/config.el -*- lexical-binding: t; -*-

(local-load! "+definitions")
(local-load! "+vars")

(defer-load! jg-bindings-core "+bindings")
(defer-load! jg-evil-ex-bindings "+evil-ex")

(use-package! org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  ;;; Custom org modules
  (dolist (flag (doom-module-context-get 'flags))
    (load! (concat "contrib/" (substring (symbol-name flag) 1)) nil t))

  ;; Add our general hooks after the submodules, so that any hooks the
  ;; submodules add run after them, and can overwrite any defaults if necessary.
  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'doom-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace'; shows a lot of false positives
             #'doom-disable-show-trailing-whitespace-h
             ;; #'+org-enable-auto-reformat-tables-h
             ;; #'+org-enable-auto-update-cookies-h
             #'+org-make-last-point-visible-h)

  (add-hook! 'org-load-hook
             #'+org-init-org-directory-h
             #'+org-init-appearance-h
             #'+org-init-agenda-h
             #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-defaults-h
             #'+org-init-capture-frame-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             #'+org-init-habit-h
             #'+org-init-hacks-h
             #'+org-init-smartparens-h
             #'+org-init-keybinds-h
             )

  (after! org-protocol
    (advice-remove 'server-visit-files #'org--protocol-detect-protocol-server))

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not byte-compile-current-file))
    (unless (doom-context-p 'reload)
      (message "`org' was already loaded by the time lang/org loaded, this may cause issues"))
    (run-hooks 'org-load-hook))

  :config
  (add-to-list 'doom-debug-variables 'org-export-async-debug)

  ;; Don't number headings with these tags
  (setq org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
        org-num-skip-tags '("noexport" "nonum"))

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)


  (add-hook 'org-open-at-point-functions #'doom-set-jump-h)
  ;; HACK For functions that dodge `org-open-at-point-functions', like
  ;;   `org-id-open', `org-goto', or roam: links.
  (advice-add #'org-mark-ring-push :around #'doom-set-jump-a)

  ;; Add the ability to play gifs, at point or throughout the buffer. However,
  ;; 'playgifs' is stupid slow and there's not much I can do to fix it; use at
  ;; your own risk.
  (add-to-list 'org-startup-options '("inlinegifs" +org-startup-with-animated-gifs at-point))
  (add-to-list 'org-startup-options '("playgifs"   +org-startup-with-animated-gifs t))
  (add-hook! 'org-mode-local-vars-hook #'+org-init-gifs-h)

  )

(use-package! toc-org ; auto-table of contents
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh")
  )

(use-package! org-crypt ; built-in
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :hook (org-reveal-start . org-decrypt-entry)
  :preface
  (after! org
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (add-hook! 'org-mode-hook
      (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

(use-package! org-clock ; built-in
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (concat doom-data-dir "org-clock-save.el"))
  (defadvice! +org--clock-load-a (&rest _)
    "Lazy load org-clock until its commands are used."
    :before '(org-clock-in
              org-clock-out
              org-clock-in-last
              org-clock-goto
              org-clock-cancel)
    (org-clock-load))
  :config
  (setq org-clock-persist 'history
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t
        ;; Remove log if task was clocked for 0:00 (accidental clocking)
        org-clock-out-remove-zero-time-clocks t
        ;; The default value (5) is too conservative.
        org-clock-history-length 20)
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package! evil-org
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

(use-package! evil-org-agenda
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd doom-leader-key) nil))

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

(use-package! org-unit-test
  :commands org-unit-test-minor-mode
  )

(use-package! ox-epub
  :after org
  )
