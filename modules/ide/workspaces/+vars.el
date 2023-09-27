;;; +vars.el -*- lexical-binding: t; -*-

;;-- compilation
(setq counsel-compile-root-functions (append counsel-compile-root-functions
                                             `(,#'+jg-workspaces-compile-root-fallback))

      compilation-always-kill t       ; kill compilation process before starting another
      compilation-ask-about-save nil  ; save all buffers on `compile'
      compilation-scroll-output 'first-error
      )

;;-- end compilation

(defvar jg-projects-switch-hook nil)

(defvar jg-projects-cmd-cache-name ".projectile-cmds")

(defvar jg-projects-doot-cmd "doot")

(spec-handling-add! compile-commands
                    '(default
                       +jg-workspaces-get-doot-commands
                       ;; counsel-compile-get-filtered-history
                       ;; counsel-compile-get-build-directories
                       ;; counsel-compile-get-make-invocation
                       ;; counsel-compile-get-make-help-invocations
                       )
                    )

;;-- projectile
(setq projectile-completion-system 'ivy
      projectile-cache-file (concat doom-cache-dir "projectile.cache")
      projectile-auto-discover nil ;; Auto-discovery is slow instead (`projectile-discover-projects-in-search-path').
      projectile-enable-caching (not noninteractive)
      projectile-globally-ignored-files '(".DS_Store" "TAGS")
      projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
      projectile-kill-buffers-filter 'kill-only-files
      projectile-known-projects-file (concat doom-cache-dir "projectile.projects")
      projectile-ignored-projects '("~/")
      projectile-ignored-project-function #'doom-project-ignored-p
      projectile-project-search-path (list (expand-file-name "~/github"))
      )

;; In the interest of performance, we reduce the number of project root marker
;; files/directories projectile searches for when resolving the project root.
(setq projectile-project-root-files-bottom-up '(".projectile"  ; projectile's root marker
                                                ".project"     ; doom project marker
                                                ".git"        ; Git VCS root dir
                                                )
      ;; This will be filled by other modules. We build this list manually so
      ;; projectile doesn't perform so many file checks every time it resolves
      ;; a project's root -- particularly when a file has no project.
      projectile-project-root-files '()
      projectile-project-root-files-top-down-recurring '("Makefile" "doot.toml" "Cargo.toml")

      compilation-buffer-name-function #'projectile-compilation-buffer-name
      compilation-save-buffers-predicate #'projectile-current-project-buffer-p

      projectile-git-submodule-command nil
      projectile-indexing-method 'hybrid
      projectile-generic-command #'+jg-projects-generic-command
      )

;;-- end projectile

;;-- persp
(setq persp-autokill-buffer-on-remove 'kill-weak
      persp-reset-windows-on-nil-window-conf nil
      persp-nil-hidden t
      persp-auto-save-fname "autosave"
      persp-save-dir (concat doom-data-dir "workspaces/")
      persp-set-last-persp-for-new-frames t
      persp-switch-to-added-buffer nil
      persp-kill-foreign-buffer-behaviour 'kill
      persp-remove-buffers-from-nil-persp-behaviour nil
      persp-auto-resume-time -1 ; Don't auto-load on startup
      persp-auto-save-opt (if noninteractive 0 1) ; auto-save on kill
      )

;; per-frame workspaces
(setq persp-init-frame-behaviour t
      persp-init-new-frame-behaviour-override nil
      persp-interactive-init-frame-behaviour-override #'+workspaces-associate-frame-fn
      persp-emacsclient-init-frame-behaviour-override #'+workspaces-associate-frame-fn
      )

(setq projectile-switch-project-action #'+jg-projects-switch)

(setq counsel-projectile-switch-project-action
      '(1 ("o" +workspaces-switch-to-project-h "open project in new workspace")
        ("O" counsel-projectile-switch-project-action "jump to a project buffer or file")
        ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
        ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
        ("D" counsel-projectile-switch-project-action-dired "open project in dired")
        ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
        ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
        ("w" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
        ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
        ("r" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
        ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
        ("C" counsel-projectile-switch-project-action-configure "run project configure command")
        ("e" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
        ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
        ("s" (lambda (project)
               (let ((projectile-switch-project-action
                      (lambda () (call-interactively #'+ivy/project-search))))
                 (counsel-projectile-switch-project-by-name project))) "search project")
        ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
        ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
        ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
        ("X" counsel-projectile-switch-project-action-org-capture "org-capture into project")))

;;-- end persp

;;-- specs
(spec-handling-add! popup
                    '(carousel
                      ("^\\*Carousel Buffers: "         :side left :ttl nil :width  0.2 :quit nil :select nil :priority 50)
                      )
                    '(proj-walk
                     ("^\\*Project Zimmerframe\\*" :side left :ttl nil :quit t :select nil :priority -50)
                     )
                    '(compilation
                      ("\\*compilation\\*" :quit t :select nil :height 0.2 :priority 20)
                      )
                    )

(spec-handling-add! file-templates
                    '(project
                     ("/doot\\.toml$" :trigger "__doot_toml" :mode conf-toml-mode)
                     )
                    )

(spec-handling-add! project-ignored
                    `(doom
                      ,(abbreviate-file-name doom-local-dir)
                      )
                    '(default
                       "^\\.idea$"
                       "^\\.vscode$"
                       "^\\.ensime_cache$"
                       "^\\.eunit$"
                       "^\\.git$"
                       "^\\.hg$"
                       "^\\.fslckout$"
                       "^_FOSSIL_$"
                       "^\\.bzr$"
                       "^_darcs$"
                       "^\\.pijul$"
                       "^\\.tox$"
                       "^\\.svn$"
                       "^\\.stack-work$"
                       "^\\.ccls-cache$"
                       "^\\.cache$"
                       "^\\.clangd$"
                    )
                    '(doot
                      ".temp"
                      )
                    )

(spec-handling-add! ibuffer-formats
                    '(workspaces
                      mark modified read-only locked
                      " " (name 18 18 :left :elide)
                      " " (size 10 10 :right)
                      " " (mode 16 16 :left :elide)
                      " " workspace)
                      )
;;-- end specs
