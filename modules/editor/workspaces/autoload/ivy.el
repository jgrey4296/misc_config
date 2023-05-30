;;; ui/workspaces/autoload/ivy.el -*- lexical-binding: t; -*-
(require 'counsel)
(require 'ivy)

;;;###autoload
(defun +workspace--ivy-rich-preview (workspace)
  (if-let (buffers (when-let (workspace (gethash workspace *persp-hash*))
                     (cl-loop for (type . rest) in (persp-window-conf workspace)
                              if (eq type 'buffer)
                              collect (car leaf)
                              else if (eq type 'leaf)
                              append (cl-loop for (type . leaf) in rest
                                              if (eq type 'buffer)
                                              collect (car leaf)))))
      (string-join buffers " ")
    "*No buffers*"))

;;;###autoload
(defun +jg-workspaces-rename (x)
  (+workspace-rename x (read-string (format "Rename %s -> : " x)))
  )

;;;###autoload
(defun +jg-workspaces-new-ring (x)
  (window-ring-new)
  )

(defun +jg-workspaces-switch (x)
  (cond ((string-equal x (+workspace-current-name))
         (+workspace-save x))
        ((+workspace-exists-p x)
         (+workspace-switch x))
        ((-contains? (bookmark-all-names) x)
         (+workspace-switch x t)
         (bookmark-jump x))
        (t (+workspace-switch x t))
        )
  )

;;;###autoload
(defun +jg-workspaces-ivy ()
    "Switch to a workspace or create a new one"
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action '(+jg-workspaces-switch)
              :caller '+jg-workspaces-ivy)
    )

;;;###autoload
(defun +jg-workspaces-get-doot-commands (&optional dir)
  " get tasks from doot, cache them, then wrap them for use in ivy "
  ;; add to counsel-compile-local-builds
  (interactive)
  (+jg-projects-doot-tasks dir)
  )

;;;###autoload
(defun +jg-workspaces-get-gradle-commands (&optional dir)
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
;;; ivy.el ends here
