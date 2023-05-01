;;; +vars.el -*- lexical-binding: t; -*-

;;-- defs

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

(defvar +workspaces-switch-project-function #'doom-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one argument: the
new project directory.")

(defvar +workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a new project.

Can be one of the following:

t           Always create a new workspace for the project
'non-empty  Only create a new workspace if the current one already has buffers
            associated with it.
nil         Never create a new workspace on project switch.")

;; FIXME actually use this for wconf bookmark system

(defvar +workspaces-data-file "_workspaces"
  "The basename of the file to store single workspace perspectives. Will be
stored in `persp-save-dir'.")

(defvar +workspace--old-uniquify-style nil)

(defvar +workspaces--indirect-buffers-to-restore nil)

;;-- end defs

(defvar jg-projects-switch-hook nil)

(defvar jg-projects-cmd-cache-name ".projectile-cmds")

(defvar jg-projects-doot-cmd "doot")

(defvar jg-projects-doit-cmd "doit")

(setq projectile-completion-system 'ivy
      counsel-compile-local-builds '(
                                     +jg-projects-get-doot-commands
                                     +jg-projects-get-gradle-commands
                                     ;; counsel-compile-get-filtered-history
                                     ;; counsel-compile-get-build-directories
                                     counsel-compile-get-make-invocation
                                     counsel-compile-get-make-help-invocations
                                     )
      )

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

;;-- specs
(spec-handling-add! popup nil
                    '(window-ring
                       ("^\\*WR Buffers: "         :side left :ttl nil :width  0.2 :quit nil :select nil :priority 50)
                    )
)

(spec-handling-add! file-templates nil
                    '(project
                     ("/doot\\.toml$" :trigger "__doot_toml" :mode conf-toml-mode)
                     ("/Makefile$"             :mode makefile-gmake-mode)
                     )
                    )

(spec-handling-add! popup nil
                    '(proj-walk
                     ("^\\*Project-Walk\\*" :side left :ttl nil :quit t :select nil :priority -50)
                     )
                    )
;;-- end specs
