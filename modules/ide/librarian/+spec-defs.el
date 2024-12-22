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
  "Register documentation lookup handlers.
Handlers
 "
  ;; TODO use upfun! here
  :struct '(librarian--doc-valid-keywords fn)
  (setq-local librarian--doc-assignments-functions      (cl-remove-duplicates (append (ensure-list (plist-get val :assignments))        librarian--doc-assignments-functions))
              librarian--doc-declaration-functions      (cl-remove-duplicates (append (ensure-list (plist-get val :declaration))        librarian--doc-declaration-functions))
              librarian--doc-definition-functions       (cl-remove-duplicates (append (ensure-list (plist-get val :definition))         librarian--doc-definition-functions))
              librarian--doc-documentation-functions    (cl-remove-duplicates (append (ensure-list (plist-get val :documentation))      librarian--doc-documentation-functions))
              librarian--doc-file-functions             (cl-remove-duplicates (append (ensure-list (plist-get val :file))               librarian--doc-file-functions))
              librarian--doc-implementations-functions  (cl-remove-duplicates (append (ensure-list (plist-get val :implementations))    librarian--doc-implementations-functions))
              librarian--doc-references-functions       (cl-remove-duplicates (append (ensure-list (plist-get val :references))         librarian--doc-references-functions))
              librarian--doc-type-definition-functions  (cl-remove-duplicates (append (ensure-list (plist-get val :type-definition))    librarian--doc-type-definition-functions))
              )
  )

(speckler-new-hook! docsets (key val)
  "Register local dash docsets"
  (setq-local dash-docs-docsets val)
  )
