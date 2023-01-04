;;; +ivys.el -*- lexical-binding: t; -*-


;;-- doit command retrieval
(defun +jg-projects-get-doit-commands (&optional dir)
  " get tasks from doit, cache them, then wrap them for use in ivy "
  ;; add to counsel-compile-local-builds
  (interactive)
  (+jg-projects-doit-tasks dir)
  )

;;-- end doit command retrieval

;;-- gradle
(defun +jg-projects-get-gradle-commands (&optional dir)
  (interactive)
  (let ((default-directory (or dir (projectile-project-root) default-directory))
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
