;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! browse-handler (key val)
  "Register browse-url handlers"
  :target browse-url-default-handlers
  :struct '(list)
  :loop 'append
  val
  )

(speckler-new! online-search (key vals)
  "Register url search providers"
  ;; :target librarian--online-provider-url-alist
  :struct '("name" "url-or-fn")
  :loop 'do
  (cl-loop for prov in vals
           do
           (librarian--online-register-provider (car prov) (upfun! (cadr prov)))
           )
  )

(speckler-new-hook! doc-lookup (keys vals)
  "Register documentation lookup handlers. "
  :struct '(plistp librarian--doc-valid-keywords handlers)
  (cl-loop for prop in librarian--doc-valid-keywords
           for fns = (plist-get vals prop)
           do
           (librarian--doc-update-handler prop
                                          (cond ((functionp fns)
                                                 (list fns))
                                                ((eq (car-safe fns) 'function)
                                                 (list (upfun! fns)))
                                                ((listp fns)
                                                 (mapcar #'upfun! fns))
                                                (t (ensure-list fns))
                                                )
                                          )
           )
  )

(speckler-new-hook! docsets (key val)
  "Register local dash docsets"
  (setq-local dash-docs-docsets val)
  )
