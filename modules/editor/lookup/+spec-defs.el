;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! browse-handler browse-url-handlers :loop 'append
                    val
                    )

(spec-handling-new! lookup-url +lookup-provider-url-alist :loop 'append
                    val
                    )

(spec-handling-new! lookup-handler nil :loop 'hook
                    (setq-local +lookup-definition-functions      (plist-get val :definition)
                                +lookup-implementations-functions (plist-get val :implementations)
                                +lookup-type-definition-functions (plist-get val :type-definition)
                                +lookup-references-functions      (plist-get val :references)
                                +lookup-documentation-functions   (plist-get val :documentation)
                                +lookup-file-functions            (plist-get val :file)
                                )
                    )

(spec-handling-new! lookup-regular nil :loop 'hook
                    ;; Val : alist of (name . url)
                    (setq-local lookup-regular-targets (append lookup-regular-targets val))
                    )

(spec-handling-new! docsets nil :loop 'hook
                    (setq-local dash-docs-docsets val)
                    )
