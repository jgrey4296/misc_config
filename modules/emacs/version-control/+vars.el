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

;; TODO Implement me
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

(setq-default vc-handled-backends '(SVN Git Hg))

(setq code-review-db-database-file (concat doom-data-dir "code-review/code-review-db-file.sqlite")
      code-review-log-file (concat doom-data-dir "code-review/code-review-error.log")
      code-review-download-dir (concat doom-data-dir "code-review/")
      )

(spec-handling-add! popup
                    '(magit
                     ("^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t :priority 200)
                     ("^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)
                     )
                    '(forge
                     ( "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
                     ("^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t)
                     )
                    '(vc
                      ("^\\*vc-diff" :select nil)   ; *vc-diff*
                      ("^\\*vc-change" :select t)   ; *vc-change-log*
                      )
                    )

(spec-handling-add! fold :form 'override
                    `(magit
                      :modes (magit-status-mode)
                      :priority 50
                      :triggers (:delete     nil
                                 :open-all   nil
                                 :close-all  nil
                                 :toggle     ,(cmd! (magit-section-toggle (magit-current-section)))
                                 :open       nil
                                 :open-rec   nil
                                 :close      nil
                                 )
                      )
                    )

(spec-handling-add! auto-modes
                    '(version-control
                      ("/\\.dockerignore\\'"  . gitignore-mode)
                      ("/\\.gitignore\\'"     . gitignore-mode)
                      ("/\\.gitconfig\\'"     . gitconfig.mode)
                      ("/git-rebase-todo\\'"  . git-rebase-mode)
                      )
                    )

(spec-handling-add! yas-extra
                    '(git-commit-mode git-commit-mode)
                    )
