;;; emacs/jg-vc/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(after! evil
  (load! "+conflict-merge-state")
  )
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! smerge-mode
  :after jg-bindings-total
  )

(use-package! magit
  :commands magit-file-delete
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

  (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    ;; Since the project likely now contains new files, best we undo the
    ;; projectile cache so it can be regenerated later.
    (projectile-invalidate-cache nil)
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))

  ;; Center the target file, because it's poor UX to have it at the bottom of
  ;; the window after invoking `magit-status-here'.
  (advice-add #'magit-status-here :after #'doom-recenter-a)

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
  (add-hook! 'magit-status-mode-hook
    (defun +magit-optimize-process-calls-h ()
      (when-let (path (executable-find magit-git-executable t))
        (setq-local magit-git-executable path))))

  (add-hook! 'magit-diff-visit-file-hook
    (defun +magit-reveal-point-if-invisible-h ()
      "Reveal the point if in an invisible region."
      (if (derived-mode-p 'org-mode)
          (org-reveal '(4))
        (require 'reveal)
        (reveal-post-command))))

  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (add-hook! 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))

  (add-hook! 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))

  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; so magit buffers can be switched to (except for process buffers)
  (add-hook! 'doom-real-buffer-functions
    (defun +magit-buffer-p (buf)
      (with-current-buffer buf
        (and (derived-mode-p 'magit-mode)
             (not (eq major-mode 'magit-process-mode))))))

  (add-hook! 'magit-section-mode-hook
    (add-hook! 'window-configuration-change-hook :local
      (defun +magit-enlargen-fringe-h ()
        "Make fringe larger in magit."
        (and (display-graphic-p)
             (derived-mode-p 'magit-section-mode)
             +magit-fringe-size
             (let ((left  (or (car-safe +magit-fringe-size) +magit-fringe-size))
                   (right (or (cdr-safe +magit-fringe-size) +magit-fringe-size)))
               (set-window-fringes nil left right))))))

  )

(use-package! forge
  :when (modulep! +forge)
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-database-file (concat doom-data-dir "forge/forge-database.sqlite"))
  (setq forge-add-default-bindings (not (modulep! :editor evil +everywhere)))
  :config
  (defadvice! +magit--forge-get-repository-lazily-a (&rest _)
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."
    :before-while #'forge-get-repository
    (file-executable-p emacsql-sqlite-executable))

  (defadvice! +magit--forge-build-binary-lazily-a (&rest _)
    "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
    :before #'forge-dispatch
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2)
      (if (not (file-executable-p emacsql-sqlite-executable))
          (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                           "See *Compile-Log* buffer for details"))
        ;; HACK Due to changes upstream, forge doesn't initialize completely if
        ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
        ;;      manually after installing it.
        (setq forge--sqlite-available-p t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
        (after! forge-topic
          (dolist (hook forge-bug-reference-hooks)
            (add-hook hook #'forge-bug-reference-setup))))))

  )

(use-package! code-review
  :when (modulep! +forge)
  :after magit
  :init
  ;; TODO This needs to either a) be cleaned up or better b) better map things
  ;; to fit
  (after! evil-collection-magit
    (dolist (binding evil-collection-magit-mode-map-bindings)
      (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
        (dolist (state states)
          (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
    (evil-set-initial-state 'code-review-mode evil-default-state))

  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-code-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review))))

(use-package! magit-todos
  :after magit
  )

(use-package! evil-collection-magit
  :when (modulep! :editor evil +everywhere)
  :defer t
  )

(use-package! evil-collection-magit-section
  :when (modulep! :editor evil +everywhere)
  :defer t
)
