;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-ibuffer-filters (make-hash-table) "Hashtable of hashtables for defining ibuffer filters")
(defvar jg-ibuffer-filter-groups (make-hash-table) "Hashtable of hashtables for defining ibuffer filter groups")
(defvar jg-ibuffer-used-filter-names '())
(defvar jg-ibuffer-used-group-names '())

(setq jg-ibuffer-never-show-regexps
      (rx (: "*" (or "Minibuf-"
                     "which-key"
                     "server"
                     "Async-native-compile-log"
                     "Echo Area"
                     "eldoc"
                     "org-src-fontification:"
                     "code-converting-work"
                     "helm candidates"
                     "Pp Eval"
                     ))
          (* any)
          )
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

;;-- popup
(setq jg-ibuffer-popup-rules
      '(
        ("^\*Ibuffer\*$"         :side right  :ttl 5 :width  0.5 :quit nil :select t :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'ibuffer jg-ibuffer-popup-rules)
  )
;;-- end popup
