;;; +vars.el -*- lexical-binding: t; -*-

(setq-default jg-projects-cmd-cache-name ".projectile-cmds"
              projectile-completion-system 'ivy

              jg-projects-walk-popup-rules '(("^\\*Project-Walk\\*" :side left :ttl nil :quit t :select nil :priority -50))

              jg-projects-doot-cmd "doot"
              jg-projects-doit-cmd "doit"
              )

;;-- popup
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'proj-walk jg-projects-walk-popup-rules)
  )

;;-- end popup

;;-- projectile
(after! projectile

  (projectile-register-project-type 'jg-completion-project '("dodo.py" "doot.toml")
                                    :project-file "dodo.py"
                                    ;; :related-files-fn #'+jg-projects-related-files-fn
                                    )
  )


;;-- end projectile

;;-- projectile compile
(setq counsel-compile-local-builds '(
                                     +jg-projects-get-doot-commands
                                     +jg-projects-get-gradle-commands
                                     ;; counsel-compile-get-filtered-history
                                     ;; counsel-compile-get-build-directories
                                     counsel-compile-get-make-invocation
                                     counsel-compile-get-make-help-invocations
                                     )
      )
;;-- end projectile compile

;;-- file templates
(after! jg-completion-templates
  ;; project
  (+jg-completion-add-file-templates
   'project
   '(
     ("/doot\\.toml$" :trigger "__doot_toml" :mode conf-toml-mode)
     ("/Makefile$"             :mode makefile-gmake-mode)
     )
   )
  )
;;-- end file templates
