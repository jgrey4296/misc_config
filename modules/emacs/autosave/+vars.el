;;; +vars.el -*- lexical-binding: t; -*-

(setq auto-save-default t
      auto-save-include-big-deletions t
      ;; Keep it out of `doom-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat doom-cache-dir "autosave/")
      tramp-auto-save-directory  (concat doom-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                                 ;; Prefix tramp autosaves to prevent conflicts with local ones
                                                 (concat auto-save-list-file-prefix "tramp-\\2") t)
                                           (list ".*" auto-save-list-file-prefix t)))
