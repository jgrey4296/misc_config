;; +defs.el -*- lexical-binding: t; -*-

(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")

(defvar +magit-fringe-size '(13 . 1)
  "Size of the fringe in magit-mode buffers.

Can be an integer or a cons cell whose CAR and CDR are integer widths for the
left and right fringe.

Only has an effect in GUI Emacs.")

(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")

(defvar +magit--pos nil)

(defvar forge-add-default-bindings nil)

(defvar evil-collection-magit-use-z-for-folds t)

(defvar evil-collection-magit-section-use-z-for-folds evil-collection-magit-use-z-for-folds)

(defvar +jg-vcs-task-hash (make-hash-table :test 'equal))

(defvar +jg-vcs-gradle-command "gradle")

(defvar +jg-vcs-gradle-command-args '())

(defvar jg-vcs-tag-file (expand-file-name "~/github/bibliography/completions/vcs_tags"))
