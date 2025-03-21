;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! browse-handler (key val)
  "Register browse-url handlers"
  :target browse-url-default-handlers
  :struct '(list)
  :loop 'append
  val
  )

(speckler-new! lookup-url (key val)
  "Register url lookup providers"
  :target librarian--online--provider-url-alist
  :struct '(list (or ("name" "url") ("name" fn "url")))
  :loop 'append
  val
  )

(speckler-new-hook! lookup-handler (keys vals)
  "Register documentation lookup handlers. "
  :struct '(plistp librarian--doc-valid-keywords handlers)
  (cl-loop for prop in librarian--doc-valid-keywords
           for fns = (plist-get vals prop)
           do
           (librarian--doc-update-handler prop
                                          (pcase fns
                                            ((and x (pred functionp)) (list x))
                                            ((and x `(function . ,_)) (list (upfun! x)))
                                            ((and x (pred listp))     (mapcar #'upfun! x))
                                            (x (ensure-list x))
                                            )
                                          )
           )
  )

(speckler-new-hook! docsets (key val)
  "Register local dash docsets"
  (setq-local dash-docs-docsets val)
  )
