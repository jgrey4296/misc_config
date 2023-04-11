;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-projects-cmd-cache-name ".projectile-cmds")
(defvar jg-projects-doot-cmd "doot")
(defvar jg-projects-doit-cmd "doit")

(setq projectile-completion-system 'ivy
      counsel-compile-local-builds '(
                                     +jg-projects-get-doot-commands
                                     +jg-projects-get-gradle-commands
                                     ;; counsel-compile-get-filtered-history
                                     ;; counsel-compile-get-build-directories
                                     counsel-compile-get-make-invocation
                                     counsel-compile-get-make-help-invocations
                                     )
      )


;;-- file spec
(after! jg-ui-reapply-hook-ready
  (+jg-snippets-add-file-spec 'project
                              '(
                                ("/doot\\.toml$" :trigger "__doot_toml" :mode conf-toml-mode)
                                ("/Makefile$"             :mode makefile-gmake-mode)
                                )
                              )
  )
;;-- end file spec

;;-- popup spec
(after! jg-popup-init
  (+jg-popup-add-spec 'proj-walk
                      '(("^\\*Project-Walk\\*" :side left :ttl nil :quit t :select nil :priority -50))
                      )
  )

;;-- end popup spec
