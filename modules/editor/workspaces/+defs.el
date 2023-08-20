;; +defs.el -*- lexical-binding: t; -*-

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
