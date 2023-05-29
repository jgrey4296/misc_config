;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-ibuffer-never-show-regexps
      (rx (: "*" (or "Minibuf-"
                     "scratch"
                     "Messages"
                     "DOC"
                     "which-key"
                     "server"
                     "Async-native-compile-log"
                     "Echo Area"
                     "eldoc"
                     "org-src-fontification:"
                     "code-converting-work"
                     "code-conversion-work"
                     "helm candidates"
                     ;; "Pp Eval"
                     "RNC Input"
                     "counsel"
                     "refs-/"
                     "Neotree"
                     )
             (* any)
             ))
      )

(setq ibuffer-show-empty-filter-groups t
      ibuffer-default-sorting-mode 'alphabetic
      ibuffer-filter-group-name-face '(:inherit (success bold))
      ibuffer-old-time 2

      jg-ibuffer-default-filter "-clutter"
      jg-ibuffer-default-group "default"
      jg-ibuffer-ivy-predicate-patterns (rx (or "*helpful"
                                                "*helm-"
                                                "doom"
                                                "*dired-log"
                                                "magit"
                                                "*Free Keys"
                                                )
                                            )
      )

;;-- popup spec
(spec-handling-add! popup
                    '(ibuffer
                      ("^\*Ibuffer\*$"         :side right  :ttl 5 :width  0.5 :quit nil :select t :priority 50)
                      )
                    )
;;-- end popup spec
