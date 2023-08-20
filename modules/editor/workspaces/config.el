;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

;; TODO replace def-project-mode!

(load! "+vars")

(defer-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")
(defer-load! jg-evil-ex-bindings "+evil-ex")


(use-package! persp-mode
  :unless noninteractive
  :commands persp-switch-to-buffer
  :hook (doom-init-ui . persp-mode)
  :config
  ;; Per-workspace `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  ;;-- hooks
  (add-hook! '(persp-mode-hook persp-after-load-state-functions)
             #'+workspaces-ensure-no-nil-workspaces-h
             )

  (add-hook! 'persp-mode-hook
             #'+workspaces-init-first-workspace-h
             #'+workspaces-init-persp-mode-h
             )

  (add-hook! 'persp-before-deactivate-functions
             #'+workspaces-save-winner-data-h
             )

  (add-hook! 'persp-activated-functions
             #'+workspaces-load-winner-data-h
    )

  ;; Fix #1973: visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)
  (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions #'doom-unreal-buffer-p)
  ;; Don't try to persist dead/remote buffers. They cause errors.
  (add-hook! 'persp-filter-save-buffers-functions
             #'+workspaces-dead-buffer-p
             #'+workspaces-remote-buffer-p
             )

  (add-hook! 'persp-after-load-state-functions
             #'+workspaces-reload-indirect-buffers-h
             )

  (after! posframe
    ;; Fix #1017: stop session persistence from restoring a broken posframe
    (add-hook! 'persp-after-load-state-functions
               #'+workspaces-delete-all-posframes-h
               ))

  ;;;; Registering buffers to perspectives
  (add-hook! 'doom-switch-buffer-hook
             #'+workspaces-add-current-buffer-h
    )

  (add-hook 'delete-frame-functions #'+workspaces-delete-associated-workspace-h)
  (add-hook 'server-done-hook #'+workspaces-delete-associated-workspace-h)

  ;; Otherwise, buffers opened via bookmarks aren't treated as "real" and are
  ;; excluded from the buffer list.
  (add-hook 'bookmark-after-jump-hook #'+workspaces-add-current-buffer-h)

  (add-hook! 'jg-projects-switch-hook
             #'+workspaces-set-project-action-fn
             #'+workspaces-switch-to-project-h
             )

  ;; tab-bar
  (add-hook! 'tab-bar-mode-hook
             #'+workspaces-set-up-tab-bar-integration-h
             )
  ;;-- end hooks

  ;;-- advice

  (defadvice! +workspaces--evil-alternate-buffer-a (&optional window)
    "Make `evil-alternate-buffer' ignore buffers outside the current workspace."
    :override #'evil-alternate-buffer
    (let* ((prev-buffers
            (if persp-mode
                (cl-remove-if-not #'persp-contain-buffer-p (window-prev-buffers)
                                  :key #'car)
              (window-prev-buffers)))
           (head (car prev-buffers)))
      (if (eq (car head) (window-buffer window))
          (cadr prev-buffers)
        head)))

  (defadvice! +workspaces-remove-dead-buffers-a (persp)
    " HACK Fixes #4196, #1525: selecting deleted buffer error when quitting Emacs
        or on some buffer listing ops. "
    :before #'persp-buffers-to-savelist
    (when (perspective-p persp)
      ;; HACK Can't use `persp-buffers' because of a race condition with its gv
      ;;      getter/setter not being defined in time.
      (setf (aref persp 2)
            (cl-delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

  ;; Don't bother auto-saving the session if no real buffers are open.
  (advice-add #'persp-asave-on-exit :around #'+workspaces-autosave-real-buffers-a)

  ;;-- end advice

  (when (modulep! :completion ivy)
    (after! ivy-rich
      (cl-callf plist-put ivy-rich-display-transformers-list
        '+workspace/switch-to
        '(:columns ((ivy-rich-candidate (:width 50))
                    (+workspace--ivy-rich-preview))))))

  ;;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; compile
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode default-directory compilation-directory
                compilation-environment compilation-arguments))

  ;; magit
  (persp-def-buffer-save/load
   :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
   :save-vars '(default-directory)
   :load-function (lambda (savelist &rest _)
                    (cl-destructuring-bind (buffer-name vars &rest _rest) (cdr savelist)
                      (magit-status (alist-get 'default-directory vars)))))

  ;; Restore indirect buffers
  (persp-def-buffer-save/load
   :tag-symbol 'def-indirect-buffer
   :predicate #'buffer-base-buffer
   :save-function (lambda (buf tag vars)
                    (list tag (buffer-name buf) vars
                          (buffer-name (buffer-base-buffer buf))))
   :load-function (lambda (savelist &rest _rest)
                    (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                        (cdr savelist)
                      (push (cons buf-name base-buf-name)
                            +workspaces--indirect-buffers-to-restore)
                      nil)))

  )

(use-package! carousel-minor-mode
  :after persp-mode
  :config
  (carousel-minor-mode 1)
  (after! ivy
    (ivy-add-actions '+jg-workspaces-ivy
                     '(("r" +jg-workspaces-rename "Rename")
                       ("l" +jg-workspaces-new-ring "new loop")
                       )
                     )
    )
  )

(use-package! project-zimmerframe
  :commands (project-zimmerframe-minor-mode zimmerframe-next)
  )

(use-package! neotree
  :disabled t
  :commands (neotree-show neotree-hide neotree-toggle neotree-dir neotree-find neo-global--with-buffer neo-global--window-exists-p)
  :config
  (after! winner
    (add-to-list 'winner-boring-buffers neo-buffer-name))
  (add-hook! 'neo-enter-hook #'+neotree-fix-cursor-h)
  )

(use-package! related-files
  :commands make-related!
  )

(use-package! compile
  :defer t
  :config
  (add-hook 'compilation-mode-hook #'hl-line-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  )

(use-package! projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :init
  (setq projectile-cache-file (concat doom-cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat doom-cache-dir "projectile.projects")
        projectile-ignored-projects '("~/")
        projectile-ignored-project-function #'doom-project-ignored-p)

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  :config
  ;; HACK: Projectile cleans up the known projects list at startup. If this list
  ;;   contains tramp paths, the `file-remote-p' calls will pull in tramp via
  ;;   its `file-name-handler-alist' entry, which is expensive. Since Doom
  ;;   already cleans up the project list on kill-emacs-hook, it's simplest to
  ;;   inhibit this cleanup process at startup (see bbatsov/projectile#1649).
  (letf! ((#'projectile--cleanup-known-projects #'ignore))
    (projectile-mode +1))
  ;; HACK: Auto-discovery and cleanup on `projectile-mode' is slow and
  ;;   premature. Let's try to defer it until it's needed.
  (add-transient-hook! 'projectile-relevant-known-projects
    (projectile--cleanup-known-projects)
    (when projectile-auto-discover
      (projectile-discover-projects-in-search-path)))

  ;; Projectile runs four functions to determine the root (in this order):
  ;;
  ;; + `projectile-root-local' -> checks the `projectile-project-root' variable
  ;;    for an explicit path.
  ;; + `projectile-root-bottom-up' -> searches from / to your current directory
  ;;   for the paths listed in `projectile-project-root-files-bottom-up'. This
  ;;   includes .git and .project
  ;; + `projectile-root-top-down' -> searches from the current directory down to
  ;;   / the paths listed in `projectile-root-files', like package.json,
  ;;   setup.py, or Cargo.toml
  ;; + `projectile-root-top-down-recurring' -> searches from the current
  ;;   directory down to / for a directory that has one of
  ;;   `projectile-project-root-files-top-down-recurring' but doesn't have a
  ;;   parent directory with the same file.
  ;;
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".project"     ; doom project marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  (push (abbreviate-file-name doom-local-dir) projectile-globally-ignored-directories)

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  ;; Support the more generic .project files as an alternative to .projectile
  (defadvice! doom--projectile-dirconfig-file-a ()
    :override #'projectile-dirconfig-file
    (cond ((file-exists-p! (or ".projectile" ".project") (projectile-project-root)))
          ((expand-file-name ".project" (projectile-project-root)))))

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +default/search-project instead")
  (put 'projectile-ripgrep 'disabled "Use +default/search-project instead")
  (put 'projectile-grep 'disabled "Use +default/search-project instead")

  ;; Treat current directory in dired as a "file in a project" and track it
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (add-hook! 'kill-emacs-hook
    (defun doom-cleanup-project-cache-h ()
      "Purge projectile cache entries that:

a) have too many files (see `doom-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private. (see `doom-projectile-cache-blacklist'),
c) are not valid projectile projects."
      (when (and (bound-and-true-p projectile-projects-cache)
                 projectile-enable-caching)
        (setq projectile-known-projects
              (cl-remove-if #'projectile-ignored-project-p
                            projectile-known-projects))
        (projectile-cleanup-known-projects)
        (cl-loop with blacklist = (mapcar #'file-truename doom-projectile-cache-blacklist)
                 for proot in (hash-table-keys projectile-projects-cache)
                 if (or (not (stringp proot))
                        (string-empty-p proot)
                        (>= (length (gethash proot projectile-projects-cache))
                            doom-projectile-cache-limit)
                        (member (substring proot 0 -1) blacklist)
                        (and doom-projectile-cache-purge-non-projects
                             (not (doom-project-p proot)))
                        (projectile-ignored-project-p proot))
                 do (doom-log "Removed %S from projectile cache" proot)
                 and do (remhash proot projectile-projects-cache)
                 and do (remhash proot projectile-projects-cache-time)
                 and do (remhash proot projectile-project-type-cache))
        (projectile-serialize-cache))))

  ;; Some MSYS utilities auto expanded the `/' path separator, so we need to prevent it.
  (when IS-WINDOWS
    (setenv "MSYS_NO_PATHCONV" "1") ; Fix path in Git Bash
    (setenv "MSYS2_ARG_CONV_EXCL" "--path-separator")) ; Fix path in MSYS2

  ;; HACK Don't rely on VCS-specific commands to generate our file lists. That's
  ;;      7 commands to maintain, versus the more generic, reliable and
  ;;      performant `fd' or `ripgrep'.
  (defadvice! doom--only-use-generic-command-a (fn vcs)
    "Only use `projectile-generic-command' for indexing project files.
And if it's a function, evaluate it."
    :around #'projectile-get-ext-command
    (if (and (functionp projectile-generic-command)
             (not (file-remote-p default-directory)))
        (funcall projectile-generic-command vcs)
      (let ((projectile-git-submodule-command
             (get 'projectile-git-submodule-command 'initial-value)))
        (funcall fn vcs))))

  ;; `projectile-generic-command' doesn't typically support a function, but my
  ;; `doom--only-use-generic-command-a' advice allows this. I do it this way so
  ;; that projectile can adapt to remote systems (over TRAMP), rather then look
  ;; for fd/ripgrep on the remote system simply because it exists on the host.
  ;; It's faster too.
  (put 'projectile-git-submodule-command 'initial-value projectile-git-submodule-command)
  (setq projectile-git-submodule-command nil
        projectile-indexing-method 'hybrid
        projectile-generic-command
        (lambda (_)
          ;; If fd exists, use it for git and generic projects. fd is a rust
          ;; program that is significantly faster than git ls-files or find, and
          ;; it respects .gitignore. This is recommended in the projectile docs.
          (cond
           ((when-let*
                ((bin (if (ignore-errors (file-remote-p default-directory nil t))
                          (cl-find-if (doom-rpartial #'executable-find t)
                                      (list "fdfind" "fd"))
                        doom-projectile-fd-binary))
                 ;; REVIEW Temporary fix for #6618. Improve me later.
                 (version (with-memoization doom-projects--fd-version
                            (cadr (split-string (cdr (doom-call-process bin "--version"))
                                                " " t))))
                 ((ignore-errors (version-to-list version))))
                (concat (format "%s . -0 -H --color=never --type file --type symlink --follow --exclude .git %s"
                                bin (if (version< version "8.3.0")
                                        "" "--strip-cwd-prefix"))
                        (if IS-WINDOWS " --path-separator=/"))))
           ;; Otherwise, resort to ripgrep, which is also faster than find
           ((executable-find "rg" t)
            (concat "rg -0 --files --follow --color=never --hidden -g!.git"
                    (if IS-WINDOWS " --path-separator=/")))
           ("find . -type f -print0"))))

  (defadvice! doom--projectile-default-generic-command-a (fn &rest args)
    "If projectile can't tell what kind of project you're in, it issues an error
when using many of projectile's command, e.g. `projectile-compile-command',
`projectile-run-project', `projectile-test-project', and
`projectile-configure-project', for instance.

This suppresses the error so these commands will still run, but prompt you for
the command instead."
    :around #'projectile-default-generic-command
    (ignore-errors (apply fn args))))

(use-package! winner
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (doom-first-buffer . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*"))

  )
