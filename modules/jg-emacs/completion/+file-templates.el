;;; +file-templates.el -*- lexical-binding: t; -*-

(message "Setting file templates dir")

;;-- file-templates control
(defun +jg-completion-add-file-templates (sym rules &optional override)
  (cl-assert (hash-table-p jg-completion-file-template-rules))
  (unless (and (gethash sym jg-completion-file-template-rules) (not override))
    (puthash sym
             (cl-loop for (head . body) in rules
                      for priority = (* -1 (or (plist-get body :priority) 0))
                      for clean    = (cl-loop for (k v) on body by #'cddr
                                              unless (eq k :priority)
                                              collect k and collect v)
                      collect (cons priority (cons head clean))
                      )
             jg-completion-file-template-rules)
    )
  )

(defun +jg-completion-file-interactive-activate ()
  (interactive)
  (+jg-completion-activate-file-templates t)
  )

(defun +jg-completion-activate-file-templates (&optional force)
  (message "%s Activating File Templates: %s"
           (if force "Force " "")
           (hash-table-keys jg-completion-file-template-rules))
  (unless (and jg-completion-file-templates-flat (not force))
    (let ((all-rules (copy-sequence (-flatten-n 1 (hash-table-values jg-completion-file-template-rules)))))
      (setq jg-completion-file-templates-flat
            (-concat (mapcar #'cdr (sort all-rules #'(lambda (x y) (< (car x) (car y)))))
                     '(("*jg-modified*"))))
      )
    )

  (when jg-completion-file-templates-flat
    (setq +file-templates-dir jg-completion-file-templates-dir
          yas-snippet-dirs (list +snippets-dir
                                 +file-templates-dir
                                 doom-snippets-dir
                                 yasnippet-snippets-dir)
          yas--default-user-snippets-dir (car yas-snippet-dirs)
          +file-templates-alist jg-completion-file-templates-flat
          )
    )

  (when force
    (yas-reload-all)
    )
  )

(define-advice yas-reload-all (:before (&rest args) +jg-yas-dir-fix)
  (+jg-completion-activate-file-templates)
  )

;;-- end file-templates control

;;-- defaults

(+jg-completion-add-file-templates
 'general
 '(("/docker-compose\\.yml$" :mode yaml-mode)
   ("/Makefile$"             :mode makefile-gmake-mode)
   ;; direnv
   ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
   ;; Markdown
   (markdown-mode)
   ;; Markdown
   (sh-mode :priority -100)
   (gitignore-mode :priority -100)
   (dockerfile-mode)
   (snippet-mode)
   )
 )
;;-- end defaults

(provide 'jg-file-templates)
