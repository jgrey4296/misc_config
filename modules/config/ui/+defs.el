;;; +defs.el -*- lexical-binding: t; -*-

(defvar mouse-wheel-down-event nil)

(defvar mouse-wheel-up-event nil)

(defvar global-hl-line-modes
  '(prog-mode text-mode conf-mode special-mode
    org-agenda-mode dired-mode)
  "What modes to enable `hl-line-mode' in.")

(defvar jg-evil-treemacs-state-map           (make-sparse-keymap "jg-evil-treemacs-state-map"))

(defvar jg-treemacs-project-map              (make-sparse-keymap "jg-treemacs-project-map"))

(defvar jg-treemacs-toggle-map               (make-sparse-keymap "jg-treemacs-toggle-map"))

(defvar jg-treemacs--fringe-indicator-bitmap (make-sparse-keymap "jg-treemacs--fringe-indicator-bitmap"))

(defvar jg-treemacs-workspace-map            (make-sparse-keymap "jg-treemacs-workspace-map"))

(defvar jg-treemacs-copy-map                 (make-sparse-keymap "jg-treemacs-copy-map"))

(defvar jg-treemacs-node-visit-map           (make-sparse-keymap "jg-treemacs-node-visit-map"))

(defvar jg-treemacs-mode-map                 (make-sparse-keymap "jg-treemacs-mode-map"))
