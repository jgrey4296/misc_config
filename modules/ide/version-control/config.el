;;; emacs/jg-vc/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+extra-config")
(when (modulep! +forge) (local-load! "+forge"))

(defer-load! jg-bindings-total "+bindings")

(defer-load! (magit jg-evil-ex-bindings) "+evil-ex")

(advice-add 'browse-at-remote--get-local-branch       :after-until #'+vc--fallback-to-master-branch-a)
(advice-add 'browse-at-remote-get-url                 :around #'+vc-support-git-timemachine-a)
(advice-add 'diff-hl-define-bitmaps                   :override #'+vc-gutter-define-thin-bitmaps-a)
(advice-add 'diff-hl-fringe-bmp-from-pos              :override #'+vc-gutter-type-at-pos-fn)
(advice-add 'diff-hl-fringe-bmp-from-type             :override #'+vc-gutter-type-at-pos-fn)
(advice-add 'git-gutter:search-near-diff-index        :override #'+vc-gutter--fix-linearity-of-hunks-a)
(advice-add 'git-timemachine--show-minibuffer-details :override #'+vc-update-header-line-a)
(advice-add 'magit-branch-and-checkout                :after #'+magit-revert-repo-buffers-deferred-a)
(advice-add 'magit-checkout                           :after #'+magit-revert-repo-buffers-deferred-a)
(advice-add 'magit-status-here                        :after #'doom-recenter-a)
(advice-add 'magit-version                            :around #'+magit--ignore-version-a)

(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'vi-tilde-fringe-mode)

(use-package! magit
  :commands (magit-file-delete magit-status)
  :defer-incrementally (dash f s with-editor git-commit package eieio transient)
  :init
  :config
  ;; The default location for git-credential-cache is in
  ;; ~/_cache_/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket (doom-glob (or (getenv "XDG_CACHE_HOME") "~/_cache_")
                                                          "git/credential/socket")))

  (add-to-list 'doom-debug-variables 'magit-refresh-verbose)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p" '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r" '("-a" "Autostash" "--autostash"))

  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.
  (add-hook! 'magit-status-mode-hook #'+magit-optimize-process-calls-h)
  (add-hook! 'magit-diff-visit-file-hook #'+magit-reveal-point-if-invisible-h)

  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (add-hook! 'magit-pre-refresh-hook #'+magit--set-window-state-h)

  (add-hook! 'magit-post-refresh-hook #'+magit--restore-window-state-h)

  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; so magit buffers can be switched to (except for process buffers)
  (add-hook! 'doom-real-buffer-functions #'+magit-buffer-p)

  (add-hook! 'magit-section-mode-hook
    (add-hook! 'window-configuration-change-hook :local
               #'+magit-enlargen-fringe-h))

  )

(use-package! git-modes
  :defer t
  :config
  (add-hook! (gitconfig-mode gitattributes-mode gitignore-mode)
             #'librarian-insert-minor-mode
             )
  )

(use-package! git-commit
  :hook (doom-first-file . global-git-commit-mode)
  :config
  ;; Enforce git commit conventions.
  ;; See https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (setq-hook! 'git-commit-mode-hook fill-column 72)

  (add-hook! 'git-commit-setup-hook #'+vc-start-in-insert-state-maybe-h)
)

(use-package! conflict-merge-state)

(use-package! browse-at-remote
  :commands browse-at-remote
  :config
  ;; It's more sensible that the user have more options. If they want line
  ;; numbers, users can request them by making a selection first. Otherwise
  ;; omitting them.
  (setq browse-at-remote-add-line-number-if-no-region-selected nil)
  ;; Opt to produce permanent links with `browse-at-remote' by default,
  ;; using commit hashes rather than branch names.
  (setq browse-at-remote-prefer-symbolic nil)

  ;; Add codeberg.org support
  ;; TODO: PR this upstream?
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^codeberg\\.org$" :type "codeberg"))
  )
