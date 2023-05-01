;;; +ivys.el -*- lexical-binding: t; -*-

(ivy-add-actions '+jg-completion-ivy-workspace
                 '(("r" +jg-workspace-rename "Rename")
                   ("l" +jg-workspace-new-ring "new loop")
                   )
                 )

(defun +jg-workspace-rename (x)
  (+workspace-rename x (read-string (format "Rename %s -> : " x)))
  )

(defun +jg-workspace-new-ring (x)
  (window-ring-new)
  )

(defun +jg-completion-ivy-workspace ()
    "Switch to a workspace or create a new one"
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action '+jg-completion-workspace-switch
              :caller '+jg-completion-ivy-workspace)
    )

(defun +jg-projects-get-doot-commands (&optional dir)
  " get tasks from doot, cache them, then wrap them for use in ivy "
  ;; add to counsel-compile-local-builds
  (interactive)
  (+jg-projects-doot-tasks dir)
  )

;;-- gradle

(defun +jg-projects-get-gradle-commands (&optional dir)
  (interactive)
  (let (
(default-directory (or dir (projectile-project-root) default-directory))
        )
    (with-temp-buffer
      (setq result-code (call-process "gradle" nil (current-buffer) nil "tasks"))
      (goto-char (point-min))
      (keep-lines "^\\w+ -")
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)-")
      (setq result-text (buffer-string))
      )
    (if (eq 0 result-code)
        (+jg-projects-annotate-cmds (split-string result-text "\n" t " \n")
                                    (lambda (x) (concat "gradle " (car (split-string x "-" t " ")))))
      '()
      )
    )
  )
;;-- end gradle
