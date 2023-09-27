;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

;; TODO replace def-project-mode!

(local-load! "+defs")
(local-load! "+vars")

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

  ;; Don't bother auto-saving the session if no real buffers are open.
  (advice-add #'persp-asave-on-exit :around #'+workspaces-autosave-real-buffers-a)

  (after! ivy-rich
    (cl-callf plist-put ivy-rich-display-transformers-list
      '+workspace/switch-to
      '(:columns ((ivy-rich-candidate (:width 50))
                  (+workspace--ivy-rich-preview)))))

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

(use-package! related-files
  :commands make-related!
  )

(use-package! compile
  :defer t
  :config
  (add-hook 'compilation-mode-hook   #'hl-line-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)

  (autoload 'comint-truncate-buffer "comint" nil t)
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
  (add-hook 'kill-emacs-hook #'doom-cleanup-project-cache-h)

  ;; Some MSYS utilities auto expanded the `/' path separator, so we need to prevent it.
  (when IS-WINDOWS
    (setenv "MSYS_NO_PATHCONV" "1") ; Fix path in Git Bash
    (setenv "MSYS2_ARG_CONV_EXCL" "--path-separator")) ; Fix path in MSYS2

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

  )

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
