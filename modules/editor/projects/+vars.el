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

;;-- project spec
(+jg-projects-add-spec 'doot '(("dodo.py" "doot.toml") :project-file "dodo.py"))
(+jg-projects-add-spec 'rust-cargo '(("Cargo.toml")                :project-file "Cargo.toml"              :compilation-dir nil :configure-command nil :compile-command "cargo build"                :test-command "cargo test"                             :install-command nil :package-command nil             :run-command "cargo run"))
(+jg-projects-add-spec 'haskell-stack '(("stack.yaml")             :project-file "stack.yaml"              :compilation-dir nil :configure-command nil :compile-command "stack build"                :test-command "stack build --test"                     :install-command nil :package-command nil             :run-command nil :test-suffix "Spec"))
(+jg-projects-add-spec 'emacs-eldev '(projectile-eldev-project-p   :project-file "Eldev"                   :compilation-dir nil :configure-command nil :compile-command "eldev compile"              :test-command "eldev test"                             :install-command nil :package-command "eldev package" :run-command "eldev emacs"))
(+jg-projects-add-spec 'emacs-cask '(("Cask")                      :project-file "Cask"                    :compilation-dir nil :configure-command nil :compile-command "cask install"               :test-command nil                                      :install-command nil :package-command nil             :run-command nil :test-suffix "-test" :test-prefix "test-"))
(+jg-projects-add-spec 'gradlew '(("gradlew")                      :project-file "gradlew"                 :compilation-dir nil :configure-command nil :compile-command "./gradlew build"            :test-command "./gradlew test"                         :install-command nil :package-command nil             :run-command nil :test-suffix "Spec"))
(+jg-projects-add-spec 'gradle '(("build.gradle")                  :project-file "build.gradle"            :compilation-dir nil :configure-command nil :compile-command "gradle build"               :test-command "gradle test"                            :install-command nil :package-command nil             :run-command nil :test-suffix "Spec"))
(+jg-projects-add-spec 'python-poetry '(("poetry.lock")            :project-file "poetry.lock"             :compilation-dir nil :configure-command nil :compile-command "poetry build"               :test-command "poetry run python -m unittest discover" :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-pipenv '( ("Pipfile")               :project-file "Pipfile"                 :compilation-dir nil :configure-command nil :compile-command "pipenv run build"           :test-command "pipenv run test"                        :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-tox '(("tox.ini")                   :project-file "tox.ini"                 :compilation-dir nil :configure-command nil :compile-command "tox -r --notest"            :test-command "tox"                                    :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-pkg '(("setup.py")                  :project-file "setup.py"                :compilation-dir nil :configure-command nil :compile-command "python setup.py build"      :test-command "python -m unittest discover"            :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-pip '(("requirements.txt")          :project-file "requirements.txt"        :compilation-dir nil :configure-command nil :compile-command "python setup.py build"      :test-command "python -m unittest discover"            :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'django '(("manage.py")                     :project-file "manage.py"               :compilation-dir nil :configure-command nil :compile-command "python manage.py runserver" :test-command "python manage.py test"                  :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'elixir '(("mix.exs")                       :project-file "mix.exs"                 :compilation-dir nil :configure-command nil :compile-command "mix compile"                :test-command "mix test"                               :install-command nil :package-command nil             :run-command nil :test-suffix "_test" :src-dir "lib/"))
(+jg-projects-add-spec 'rebar '(("rebar.config")                   :project-file "rebar.config"            :compilation-dir nil :configure-command nil :compile-command "rebar3 compile"             :test-command "rebar3 do eunit,ct"                     :install-command nil :package-command nil             :run-command nil :test-suffix "_SUITE"))
(+jg-projects-add-spec 'dotnet-sln '(("src")                       :project-file "?*.sln"                  :compilation-dir nil :configure-command nil :compile-command "dotnet build"               :test-command "dotnet test"                            :install-command nil :package-command nil             :run-command "dotnet run"))
(+jg-projects-add-spec 'dotnet '(projectile-dotnet-project-p       :project-file ("?*.csproj" "?*.fsproj") :compilation-dir nil :configure-command nil :compile-command "dotnet build"               :test-command "dotnet test"                            :install-command nil :package-command nil             :run-command "dotnet run"))
(+jg-projects-add-spec 'haskell-cabal '(projectile-cabal-project-p :project-file nil                       :compilation-dir nil :configure-command nil :compile-command "cabal build"                :test-command "cabal test"                             :install-command nil :package-command nil             :run-command "cabal run" :test-suffix "Spec"))
;;-- end project spec

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
