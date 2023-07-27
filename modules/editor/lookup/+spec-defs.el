;;; +spec-defs.el -*- lexical-binding: t; -*-

(setq-default +lookup-definition-functions      nil
              +lookup-implementations-functions nil
              +lookup-type-definition-functions nil
              +lookup-references-functions      nil
              +lookup-documentation-functions   nil
              +lookup-file-functions            nil
              +lookup-assignments-functions     nil
              )


(spec-handling-new! browse-handler browse-url-handlers :loop 'append
                    val
                    )

(spec-handling-new! lookup-url +lookup-provider-url-alist :loop 'append
                    val
                    )

(spec-handling-new! lookup-handler nil :loop 'hook
                    :struct '(:definition fn :implementation fn :type-definition nf :references fn :documentation fn :file fn :assignments fn)
                    (setq-local +lookup-definition-functions      (cl-remove-duplicates (append (ensure-list (plist-get val :definition))         +lookup-definition-functions))
                                +lookup-implementations-functions (cl-remove-duplicates (append (ensure-list (plist-get val :implementations))    +lookup-implementations-functions))
                                +lookup-type-definition-functions (cl-remove-duplicates (append (ensure-list (plist-get val :type-definition))    +lookup-type-definition-functions))
                                +lookup-references-functions      (cl-remove-duplicates (append (ensure-list (plist-get val :references))         +lookup-references-functions))
                                +lookup-documentation-functions   (cl-remove-duplicates (append (ensure-list (plist-get val :documentation))      +lookup-documentation-functions))
                                +lookup-file-functions            (cl-remove-duplicates (append (ensure-list (plist-get val :file))               +lookup-file-functions))
                                +lookup-assignments-functions     (cl-remove-duplicates (append (ensure-list (plist-get val :assignments))        +lookup-assignments-functions))
                                )
                    )

(spec-handling-new! lookup-regular nil :loop 'hook
                    ;; Val : alist of (name . url)
                    (setq-local lookup-regular-targets (append lookup-regular-targets val))
                    )

(spec-handling-new! docsets nil :loop 'hook
                    (setq-local dash-docs-docsets val)
                    )
