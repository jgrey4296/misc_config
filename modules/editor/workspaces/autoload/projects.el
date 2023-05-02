;;; projects.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-projects-switch ()
  (run-hooks 'jg-projects-switch-hook)
  )

;;;###autoload
(defun +jg-projects-generic-command (_)

  ;; If fd exists, use it for git and generic projects. fd is a rust
  ;; program that is significantly faster than git ls-files or find, and
  ;; it respects .gitignore. This is recommended in the projectile docs.
  (cond
   ((when-let*
        ((bin (if (ignore-errors (file-remote-p default-directory nil t))
                  (cl-find-if (doom-rpartial #'executable-find t)
                              (list "fdfind" "fd"))
                doom-projectile-fd-binary))
         ;; REVIEW Temporary fix for #6618. Improve me later.
         (version (with-memoization doom-projects--fd-version
                    (cadr (split-string (cdr (doom-call-process bin "--version"))
                                        " " t))))
         ((ignore-errors (version-to-list version))))
      (concat (format "%s . -0 -H --color=never --type file --type symlink --follow --exclude .git %s"
                      bin (if (version< version "8.3.0")
                              "" "--strip-cwd-prefix"))
              (if IS-WINDOWS " --path-separator=/"))))
   ;; Otherwise, resort to ripgrep, which is also faster than find
   ((executable-find "rg" t)
    (concat "rg -0 --files --follow --color=never --hidden -g!.git"
            (if IS-WINDOWS " --path-separator=/")))
   ("find . -type f -print0")))
