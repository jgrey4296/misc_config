;;; ui/popup/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+modes")
(load! "+macros")
(load! "+hacks")
(load! "+specs")
(load! "ivy/+ivy")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(spec-handling-new! popup display-buffer-alist t append
                    (cl-loop for rule in val
                             collect
                             (cons (* -1 (or (plist-get rule :priority) 0))
                                   (+popup-make-rule (car rule) (cdr rule))
                                   )
                             )
                    )
(spec-handling-add! popup nil
                    ('defaults
                      ("*jg-customised*" :priority -100)
                      )
                    )

(add-hook! 'doom-init-ui-hook   #'+popup-mode 'append)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           ;; #'+popup-set-modeline-on-enable-h
           ;; #'+popup-unset-modeline-on-disable-h
           )
