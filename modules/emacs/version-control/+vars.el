;; -*- mode:emacs-lisp; lexical-binding: t; -*-

;;-- defs

(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")

(defvar +magit-fringe-size '(13 . 1)
  "Size of the fringe in magit-mode buffers.

Can be an integer or a cons cell whose CAR and CDR are integer widths for the
left and right fringe.

Only has an effect in GUI Emacs.")

(defvar +magit--pos nil)
(defvar forge-add-default-bindings nil)
(defvar evil-collection-magit-use-z-for-folds t)
(defvar evil-collection-magit-section-use-z-for-folds evil-collection-magit-use-z-for-folds)

(defvar +jg-vcs-task-hash (make-hash-table :test 'equal))
(defvar +jg-vcs-gradle-command "gradle")
(defvar +jg-vcs-gradle-command-args '())
;;-- end defs

(defvar jg-vcs-tag-file (expand-file-name "~/github/jgrey4296.github.io/resources/completions/vcs_tags"))

(setq transient-default-level 5
      magit-diff-refine-hunk t               ;; show granular diffs in selected hunk
      magit-save-repository-buffers nil      ;; Don't autosave repo buffers.
      magit-revision-insert-related-refs nil ;; Don't display parent/related refs in commit buffers
      magit-auto-revert-mode nil

      ;; Must be set early to prevent ~/.config/emacs/transient from being created
      transient-levels-file  (concat doom-data-dir "transient/levels")
      transient-values-file  (concat doom-data-dir "transient/values")
      transient-history-file (concat doom-data-dir "transient/history")

      magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?" ;; make colon optional
      )

;; Magit uses `magit-display-buffer-traditional' to display windows, by
;; default, which is a little primitive. `+magit-display-buffer' marries
;; `magit-display-buffer-fullcolumn-most-v1' with
;; `magit-display-buffer-same-window-except-diff-v1', except:
;;
;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
;;    screen are opened as popups.
;; 2. The status screen isn't buried when viewing diffs or logs from the
;;    status screen.
(setq transient-display-buffer-action '(display-buffer-below-selected)
      magit-display-buffer-function #'+magit-display-buffer-fn
      magit-bury-buffer-function    #'magit-mode-quit-window
      )

(setq code-review-db-database-file (concat doom-data-dir "code-review/code-review-db-file.sqlite")
      code-review-log-file (concat doom-data-dir "code-review/code-review-error.log")
      code-review-download-dir (concat doom-data-dir "code-review/")
      )

(spec-handling-add! popup t
                    ('magit
                     ("^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t :priority 200)
                     )
                    ('forge
                     ( "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
                     ("^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t)
                     )
                    )

(spec-handling-add! lookup-regular nil
                    ((magit-mode magit-status-mode magit-commit-mode)
                     ("Git" . "https://git-scm.com/doc")
                     ("Github" . "https://docs.github.com/en")
                     )
                    )