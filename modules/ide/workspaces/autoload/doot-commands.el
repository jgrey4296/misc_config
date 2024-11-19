;;; compile-commands.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-workspaces-get-doot-commands (&optional dir)
  " get tasks from doot, cache them, then wrap them for use in ivy "
  ;; add to counsel-compile-local-builds
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (doot-toml (f-join root "doot.toml"))
               (dt-exists (f-exists? doot-toml))
               )
    (+jg-projects-doot-tasks)
    )
  )

;;;###autoload
(defun +jg-projects-clean(arg)
  (interactive "P")
  (-when-let* ((root (projectile-project-root))
               (doot-toml (f-join root "doot.toml"))
               (dt-exists (f-exists? doot-toml))
               (default-directory root)
               )
    (let ((targets (+jg-projects-doot-tasks nil (lambda (x) (concat jg-projects-doot-cmd " clean " (when arg "-c ") (car (split-string x" " t " "))))))
          (counsel-compile--current-build-dir (or (counsel--compile-root) default-directory))
          )
      (ivy-read "Clean Task: " ivy-opts
                :action #'counsel-compile--action
                :caller 'jg-projects-clean)
      )
    )
  )

;;;###autoload
(defun +jg-projects-doot-tasks (&optional act-fn int-fn)
  " check for cache, if cache is newer than dodo file, use that, else run doit list "
  (let ((default-directory (or (projectile-project-root) default-directory))
        (act-fn (or act-fn (lambda (x) (concat jg-projects-doot-cmd " " (car (split-string x" " t " "))))))
        skip
        )
    (cond ((not (executable-find "doot"))
           (message "No Doot Command in Path")
           (setq skip t))
          ((not (f-exists? "doot.toml"))
           (message "No doot.toml Available")
           (setq skip t))
          ((and (f-exists? ".tasks_cache")
                (time-less-p (f-modification-time "doot.toml") (f-modification-time ".tasks_cache")))
           ;; No cache/out of date, so make it
           (message "Creating Cache")
           (+jg-projects-cache-tasks jg-projects-doot-cmd "list"))
          )

    (when (and (not skip) (f-exists? ".tasks_cache"))
      (with-temp-buffer
        (insert-file-contents ".tasks_cache")
        (+jg-eval--annotate-cmds (split-string (buffer-string) "\n" t " \n")
                                    act-fn int-fn
                                    )
        )
      )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 04, 2024
;; Modified:   November 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; compile-commands.el ends here
