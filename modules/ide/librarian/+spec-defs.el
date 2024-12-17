;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! browse-handler
                    "Register browse-url handlers"
                    :target browse-url-default-handlers
                    :loop 'append
                    val
                    )

(spec-handling-new! lookup-url
                    "Register url lookup providers"
                    :target librarian--online--provider-url-alist
                    :loop 'append
                    val
                    )

(spec-handling-new-hook! lookup-handler
                         "Register documentation lookup handlers "
                         :struct '(:definition fn :implementation fn :type-definition nf :references fn :documentation fn :file fn :assignments fn :declaration fn)
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

(spec-handling-new-hook! docsets
                         "Register local dash docsets"
                         (setq-local dash-docs-docsets val)
                         )
