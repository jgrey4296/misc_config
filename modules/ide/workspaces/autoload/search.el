;;; search.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defun jg-workspaces--replace-string (substr)
  (cond ((string= substr " ")
         "  ")
        ((string= substr "|")
         "\\\\\\\\|")
        (t
         (concat "\\\\" substr))
        )
  )

;;;###autoload
(defun +jg-workspaces-search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))
         (let ((projectile-project-root nil))
           (if current-prefix-arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             (doom-project-root default-directory)))))
  (+ivy/project-search nil symbol dir))

;;;###autoload
(cl-defun +jg-workspaces-search (&key query in all-files (recursive t) prompt args)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive)
  (let* ((this-command 'counsel-rg)
         (project-root (or (projectile-project-root) default-directory))
         (directory (or in project-root))
         (args (concat (if all-files
                           " --hidden -g!.git " ;; Hidden, but not in .git
                           ,(format "--ignore-file=%s" jg-counsel-ignore-file))
                       (unless recursive " --maxdepth 1")
                       (mapconcat #'shell-quote-argument args " ")
                       ))
         (query (or query (when (doom-region-active-p)
                            (replace-regexp-in-string
                             "[! |]"
                             #'jg-workspaces--replace-string
                             (rxt-quote-pcre (doom-thing-at-point-or-region))))
                    ))
         (prompt (or prompt
                     (format "Search project [%s]: "
                             (cond ((equal directory default-directory)
                                    "./")
                                   ((equal directory project-root)
                                    (projectile-project-name))
                                   ((file-relative-name directory project-root)))
                             (string-trim args)))
                 )
         )
    (setq deactivate-mark t)
    (counsel-rg query directory args prompt)
    )
  )

;;;###autoload
(defun +jg-workspaces-search-project (&optional arg initial-query directory)
  (interactive "P")
  (+jg-workspaces-search :query initial-query :in directory :all-files arg)
  )

;;;###autoload
(defun +jg-workspaces-find-test-file (&optional arg directory)
  "Find a Test file in the project"
  (interactive "P")
  (if (and (eq projectile-require-project-root 'prompt) (not (projectile-project-p)))
      (counsel-projectile-find-file-action-switch-project))
  (projectile-maybe-invalidate-cache arg)

  (let* ((project-files (projectile-current-project-files))
         (files (and dwim (projectile-select-files project-files))))
    (ivy-read (projectile-prepend-project-name "Find file: ")
              (or files project-files)
              :matcher #'counsel-projectile--find-file-matcher
              :require-match t
              :sort counsel-projectile-sort-files
              :action counsel-projectile-find-file-action
              :caller 'counsel-projectile-find-file
              )
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    August 17, 2024
;; Modified:   August 17, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; search.el ends here
