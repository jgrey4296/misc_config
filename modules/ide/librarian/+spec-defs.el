;;; +spec-defs.el -*- lexical-binding: t; -*-

(setq-default librarian-definition-functions      nil
              librarian-declaration-functions     nil
              librarian-implementations-functions nil
              librarian-type-definition-functions nil
              librarian-references-functions      nil
              librarian-documentation-functions   nil
              librarian-file-functions            nil
              librarian-assignments-functions     nil
              )


(spec-handling-new! browse-handler browse-url-default-handlers :loop 'append
                    val
                    )

(spec-handling-new! lookup-url librarian-online--provider-url-alist :loop 'append
                    val
                    )

(spec-handling-new! lookup-handler nil :loop 'hook
                    :struct '(:definition fn :implementation fn :type-definition nf :references fn :documentation fn :file fn :assignments fn :declaration fn)
                    (setq-local librarian-assignments-functions      (cl-remove-duplicates (append (ensure-list (plist-get val :assignments))        librarian-assignments-functions))
                                librarian-definition-functions       (cl-remove-duplicates (append (ensure-list (plist-get val :definition))         librarian-definition-functions))
                                librarian-declaration-functions      (cl-remove-duplicates (append (ensure-list (plist-get val :declaration))       librarian-declaration-functions))
                                librarian-documentation-functions    (cl-remove-duplicates (append (ensure-list (plist-get val :documentation))      librarian-documentation-functions))
                                librarian-file-functions             (cl-remove-duplicates (append (ensure-list (plist-get val :file))               librarian-file-functions))
                                librarian-implementations-functions  (cl-remove-duplicates (append (ensure-list (plist-get val :implementations))    librarian-implementations-functions))
                                librarian-references-functions       (cl-remove-duplicates (append (ensure-list (plist-get val :references))         librarian-references-functions))
                                librarian-type-definition-functions  (cl-remove-duplicates (append (ensure-list (plist-get val :type-definition))    librarian-type-definition-functions))
                                )
                    )

(defvar librarian-regular--targets nil)
(spec-handling-new! lookup-regular nil :loop 'hook
                    ;; Val : alist of (name . url)
                    (setq-local librarian-regular--targets (append librarian-regular--targets val))
                    )

(spec-handling-new! docsets nil :loop 'hook
                    (setq-local dash-docs-docsets val)
                    )

(spec-handling-new! tagging nil :loop 'hook
                    (setq-local librarian-tagging-mode-handlers
                                (list :new (plist-get val :new)
                                      :set (plist-get val :set)
                                      :get (plist-get val :get)
                                      :buff (plist-get val :buff)
                                      )
                                )
                    )
