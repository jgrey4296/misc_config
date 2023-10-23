;; -*- mode:emacs-lisp; lexical-binding: t; -*-

(setq transient-default-level 5
      magit-diff-refine-hunk t               ;; show granular diffs in selected hunk
      magit-save-repository-buffers nil      ;; Don't autosave repo buffers.
      magit-revision-insert-related-refs nil ;; Don't display parent/related refs in commit buffers
      magit-auto-revert-mode nil
      magit-clone-always-transient t

      magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?" ;; make colon optional

      emacsql-sqlite-executable (executable-find "sqlite3")
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

(setq diff-hl-draw-borders nil)

(setq-default vc-handled-backends '(SVN Git Hg))

(setq code-review-db-database-file (concat doom-data-dir "code-review/code-review-db-file.sqlite")
      code-review-log-file (concat doom-data-dir "code-review/code-review-error.log")
      code-review-download-dir (concat doom-data-dir "code-review/")
      )

(spec-handling-add! popup :form 'override
                    '(magit
                     ("^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)
                     ("^magit-todos-list" :select nil :side right :ttl nil :quit t :width 80 :priority 180)
                     ;; ("^magit:" :select nil :side left :ttl nil :quit t :width 80 :priority 180)
                     ("^\\(?:\\*magit:\\|magit:\\)" :ignore t :priority 150)
                     ("^\\(?:magit-diff:\\|COMMIT_EDITMSG\\)" :ignore t :priority 200)
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

(spec-handling-add! fold
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
