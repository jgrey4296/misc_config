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

(speckler-new-hook! lookup-handler (key val)
  "Register documentation lookup handlers. "
  ;; TODO use upfun! here
  :struct '(librarian--doc-valid-keywords fn)
  (cl-loop for prop in librarian--doc-valid-keywords
           for fns = (plist-get val prop)
           do
           (librarian--doc-update-handler prop (mapcar #'upfun! (ensure-list fns)))
           )
    )

(speckler-new-hook! docsets (key val)
  "Register local dash docsets"
  (setq-local dash-docs-docsets val)
  )
