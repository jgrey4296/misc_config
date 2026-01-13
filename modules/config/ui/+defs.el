;;; +defs.el -*- lexical-binding: t; -*-

(defvar mouse-wheel-down-event nil)

(defvar mouse-wheel-up-event nil)

(defvar global-hl-line-modes
  '(prog-mode text-mode conf-mode special-mode
    org-agenda-mode dired-mode)
  "What modes to enable `hl-line-mode' in.")


(def-named-keymap! jg-evil-treemacs-state-map :sparse t)

(def-named-keymap! jg-treemacs-project-map    :sparse t)

(def-named-keymap! jg-treemacs-toggle-map     :sparse t)

(def-named-keymap! jg-treemacs--fringe-indicator-bitmap :sparse t)

(def-named-keymap! jg-treemacs-workspace-map  :sparse t)

(def-named-keymap! jg-treemacs-copy-map       :sparse t)

(def-named-keymap! jg-treemacs-node-visit-map  :sparse t)

(def-named-keymap! jg-treemacs-mode-map        :sparse t)
