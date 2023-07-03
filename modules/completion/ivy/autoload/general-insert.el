;;; general-insert.el -*- lexical-binding: t; -*-

(defvar jg-ivy-general-insert-sub-ivys (make-hash-table :test 'equal)
  "hash table mapping insertType:str -> insertIvy:fn ")

;;;###autoload
(defun +jg-ivy-general-insert ()
  (interactive)
  (ivy-read "Insert: " (hash-table-keys jg-ivy-general-insert-sub-ivys)
            :require-match t
            :action #'+jg-ivy-call-sub-ivy)

  )

(defun +jg-ivy-call-sub-ivy (selected)
  " Call a sub ivy registered in jg-ivy-general-insert-sub-ivys "
  (-if-let (sub-ivy (gethash selected jg-ivy-general-insert-sub-ivys))
      (funcall sub-ivy)
    )
)
