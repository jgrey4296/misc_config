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

;;;###autoload
(defmacro +jg-ivy-general-insert-macro (name contents prompty insertor)
  " easily make a new general inserter ivy "
  (let ((insert-fn (lambda (x) insertor)))
    '(progn
       (defvar general-insert-{name}-values {contents})
       (defun general-insert-{name}-ivy ()
         (interactive)
         (let ((val (ivy-read prompt
                              general-insert-{name}-values
                              :require-match t)))
           (insert (funcall ,insert-fn val))
           )

         )
       )

    )
  )
