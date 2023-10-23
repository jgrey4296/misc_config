;;; emacs/jg-vc/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")
(defer-load! (magit jg-evil-ex-bindings) "+evil-ex")


(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'vi-tilde-fringe-mode)

(use-package! smerge-mode
  :after jg-bindings-total
  )

(use-package! magit
  :commands (magit-file-delete magit-status)
  :defer-incrementally (dash f s with-editor git-commit package eieio transient)
  :init
  :config
  ;; The default location for git-credential-cache is in
  ;; ~/.cache/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket (doom-glob (or (getenv "XDG_CACHE_HOME")
                                                              "~/.cache/")
                                                          "git/credential/socket")))

  (add-to-list 'doom-debug-variables 'magit-refresh-verbose)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

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

(use-package! magit-todos
  :after magit
  )

(use-package! forge
  :after magit
  )

(use-package! evil-collection-magit
  :defer t
  )

(use-package! evil-collection-magit-section
  :defer t
)

(use-package! git-gutter
  :commands git-gutter:revert-hunk git-gutter:stage-hunk git-gutter:previous-hunk git-gutter:next-hunk
  :init
  (add-hook! 'find-file-hook #'+vc-gutter-init-maybe-h)

  ;; UX: Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;;   syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;;   global minor modes gets called for new buffers while they are still in
  ;;   `fundamental-mode', before a major mode has been assigned. I don't know
  ;;   why this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config

  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (add-hook! '(doom-escape-hook doom-switch-window-hook)
             :append
             #'+vc-gutter-update-h
             )
  ;; UX: update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  )

(use-package! git-gutter-fringe
  :after fringe
  :config
;; Redefine fringe bitmaps to take up only half the horizontal space
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  )

(use-package! git-timemachine
  :commands (git-timemachine-toggle)
  :config
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t)

  (after! evil
    ;; Rehash evil keybindings so they are recognized
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

  (require 'magit-blame)
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

(use-package! conflict-merge-state)

(use-package! treemacs-magit)
