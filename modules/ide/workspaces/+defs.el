;; +defs.el -*- lexical-binding: t; -*-

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

(defvar +workspaces-switch-project-function #'+workspaces-open-project-root
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

(defvar +workspace--old-uniquify-style nil)

(defvar +workspaces--indirect-buffers-to-restore nil)

(defvar-local jg-workspaces-find-buff-fn nil)

(defvar jg-projects-switch-hook nil)

(defvar jg-projects-cmd-cache-name ".projectile-cmds")

(defvar jg-projects-doot-cmd "doot")

(defvar jg-projects-related-dir-file ".related")

(defvar jg-workspaces-transient-buffer-name "*workspace-transient*")
