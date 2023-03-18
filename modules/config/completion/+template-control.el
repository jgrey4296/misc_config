;; -*- mode:emacs-lisp; no-byte-compile: t;  -*-

(defvar jg-completion-code-templates-dir    (expand-file-name "templates/code" doom-user-dir))
(defvar jg-completion-file-templates-dir    (expand-file-name "templates/files" doom-user-dir))
(defvar jg-completion-project-templates-dir (expand-file-name "templates/projects" doom-user-dir))

(defvar jg-completion-file-template-rules (make-hash-table))
(defvar jg-completion-file-templates-flat nil)

;;-- main control
(defun +jg-completion-activate-templates (&optional force)
  " Activate stored file templates, and ensure the correct snippet directories are set  "
  (unless (and jg-completion-file-templates-flat (not force))
    (let ((all-rules (copy-sequence (-flatten-n 1 (hash-table-values jg-completion-file-template-rules)))))
      (setq jg-completion-file-templates-flat
            (-concat (mapcar #'cdr (sort all-rules #'(lambda (x y) (< (car x) (car y)))))
                     '(("*jg-modified*"))))
      )
    )

  (when jg-completion-file-templates-flat
    (message "Activating File Templates: %s" (hash-table-keys jg-completion-file-template-rules))
    (setq +file-templates-dir jg-completion-file-templates-dir
          +snippets-dir jg-completion-code-templates-dir
          yas-snippet-dirs (list +snippets-dir
                                 +file-templates-dir
                                 doom-snippets-dir
                                 yasnippet-snippets-dir)
          yas--default-user-snippets-dir jg-completion-file-templates-dir
          +file-templates-alist jg-completion-file-templates-flat
          )
    )

  (when force
    (yas-reload-all)
    )
  )

(define-advice yas-reload-all (:before (&rest args) +jg-yas-dir-fix)
  (+jg-completion-activate-templates)
  )

;;-- end main control

;;-- file-templates control
(defun +jg-completion-add-file-templates (sym rules &optional override)
  " define a set of file template rules, under the symbol `sym` to avoid duplicates  "
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
  (+jg-completion-activate-templates t)
  )

;;-- end file-templates control

;;-- yasnippet
(defun +jg-completion-snippet--completing-read-uuid (prompt all-snippets &rest args)
  "Custom formatter for yasnippet, to display groups of snippets "
  (let* ((snippet-data (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                                    (hash-table-values yas--tables)
                                                                                  (yas--get-snippet-tables)))

                                unless (null (yas--template-load-file tpl))
                                for txt = (format "%-25s%-30s%s"
                                                  (yas--template-key tpl)
                                                  (yas--template-name tpl)
                                                  (abbreviate-file-name (yas--template-load-file tpl)))
                                collect
                                `(,txt . ,(yas--template-uuid tpl))))
        (selected-value (apply #'completing-read prompt snippet-data args)))
  (alist-get selected-value snippet-data nil nil 'equal)))

;;-- end yasnippet

(provide 'jg-completion-templates)
