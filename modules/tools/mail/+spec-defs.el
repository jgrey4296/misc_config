;; +spec-defs.el -*- lexical-binding: t; -*-

(defun +jg-mail-mu4e-match (maildir msg)
  (when msg
    (string-prefix-p maildir (mu4e-message-field msg :maildir) t))
  )


(spec-handling-new! mu4e-header
                    ":function : (lambda (ms) -> str)"
                    :target mu4e-header-info-custom
                    :struct '(:{key} :name :shortname :help :function :sortable)
                    :loop 'append
                    val
                    )

(spec-handling-new! mail-accounts
                    "registers mu contexts, see make-mu4e-context"
                    :target mu4e-contexts
                    :struct '(:name :maildir :vars :mu-vars :smtp-vars)
                    :loop 'append
                    (cl-loop for ctx in val
                             when (not (null ctx))
                             with loaded = (require 'mu4e-context)
                             collect (make-mu4e-context
                                      :name (plist-get ctx :name)
                                      :vars (append (plist-get ctx :vars)
                                                    (plist-get ctx :mu-vars)
                                                    (plist-get ctx :smtp-vars))
                                      :leave-func 'mu4e-clear-caches
                                      :match-func (-partial #'+jg-mail-mu4e-match
                                                            (plist-get ctx :maildir))
                                      )
                             )
                    )
