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
                     )
             (* any)
             ))
      )

(setq ibuffer-show-empty-filter-groups t
      ibuffer-default-sorting-mode 'alphabetic
      ibuffer-filter-group-name-face '(:inherit (success bold))

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
(after! jg-ui-reapply-hook-ready
  (+jg-popup-add-spec 'ibuffer
                         '(
                           ("^\*Ibuffer\*$"         :side right  :ttl 5 :width  0.5 :quit nil :select t :priority 50)
                           ))
  )
;;-- end popup spec
