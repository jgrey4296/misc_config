;;; +defs.el -*- lexical-binding: t; -*-

(defvar mouse-wheel-down-event nil)

(defvar mouse-wheel-up-event nil)

(defvar global-hl-line-modes
  '(prog-mode text-mode conf-mode special-mode
    org-agenda-mode dired-mode)
  "What modes to enable `hl-line-mode' in.")
