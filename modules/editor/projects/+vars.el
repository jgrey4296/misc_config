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

;;-- project spec
(+jg-projects-add-spec 'doot '(("dodo.py" "doot.toml") :project-file "dodo.py"))
(+jg-projects-add-spec 'rust-cargo '(("Cargo.toml")                :project-file "Cargo.toml"              :compilation-dir nil :configure nil :compile "cargo build"                :test "cargo test"                             :install nil :package nil             :run "cargo run"))
(+jg-projects-add-spec 'haskell-stack '(("stack.yaml")             :project-file "stack.yaml"              :compilation-dir nil :configure nil :compile "stack build"                :test "stack build --test"                     :install nil :package nil             :run nil :test-suffix "Spec"))
(+jg-projects-add-spec 'emacs-eldev '(projectile-eldev-project-p   :project-file "Eldev"                   :compilation-dir nil :configure nil :compile "eldev compile"              :test "eldev test"                             :install nil :package "eldev package" :run "eldev emacs"))
(+jg-projects-add-spec 'emacs-cask '(("Cask")                      :project-file "Cask"                    :compilation-dir nil :configure nil :compile "cask install"               :test nil                                      :install nil :package nil             :run nil :test-suffix "-test" :test-prefix "test-"))
(+jg-projects-add-spec 'gradlew '(("gradlew")                      :project-file "gradlew"                 :compilation-dir nil :configure nil :compile "./gradlew build"            :test "./gradlew test"                         :install nil :package nil             :run nil :test-suffix "Spec"))
(+jg-projects-add-spec 'gradle '(("build.gradle")                  :project-file "build.gradle"            :compilation-dir nil :configure nil :compile "gradle build"               :test "gradle test"                            :install nil :package nil             :run nil :test-suffix "Spec"))
(+jg-projects-add-spec 'python-poetry '(("poetry.lock")            :project-file "poetry.lock"             :compilation-dir nil :configure nil :compile "poetry build"               :test "poetry run python -m unittest discover" :install nil :package nil             :run nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-pipenv '( ("Pipfile")               :project-file "Pipfile"                 :compilation-dir nil :configure nil :compile "pipenv run build"           :test "pipenv run test"                        :install nil :package nil             :run nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-tox '(("tox.ini")                   :project-file "tox.ini"                 :compilation-dir nil :configure nil :compile "tox -r --notest"            :test "tox"                                    :install nil :package nil             :run nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-pkg '(("setup.py")                  :project-file "setup.py"                :compilation-dir nil :configure nil :compile "python setup.py build"      :test "python -m unittest discover"            :install nil :package nil             :run nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'python-pip '(("requirements.txt")          :project-file "requirements.txt"        :compilation-dir nil :configure nil :compile "python setup.py build"      :test "python -m unittest discover"            :install nil :package nil             :run nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'django '(("manage.py")                     :project-file "manage.py"               :compilation-dir nil :configure nil :compile "python manage.py runserver" :test "python manage.py test"                  :install nil :package nil             :run nil :test-suffix "_test" :test-prefix "test_"))
(+jg-projects-add-spec 'elixir '(("mix.exs")                       :project-file "mix.exs"                 :compilation-dir nil :configure nil :compile "mix compile"                :test "mix test"                               :install nil :package nil             :run nil :test-suffix "_test" :src-dir "lib/"))
(+jg-projects-add-spec 'rebar '(("rebar.config")                   :project-file "rebar.config"            :compilation-dir nil :configure nil :compile "rebar3 compile"             :test "rebar3 do eunit,ct"                     :install nil :package nil             :run nil :test-suffix "_SUITE"))
(+jg-projects-add-spec 'dotnet-sln '(("src")                       :project-file "?*.sln"                  :compilation-dir nil :configure nil :compile "dotnet build"               :test "dotnet test"                            :install nil :package nil             :run "dotnet run"))
(+jg-projects-add-spec 'dotnet '(projectile-dotnet-project-p       :project-file ("?*.csproj" "?*.fsproj") :compilation-dir nil :configure nil :compile "dotnet build"               :test "dotnet test"                            :install nil :package nil             :run "dotnet run"))
(+jg-projects-add-spec 'haskell-cabal '(projectile-cabal-project-p :project-file nil                       :compilation-dir nil :configure nil :compile "cabal build"                :test "cabal test"                             :install nil :package nil             :run "cabal run" :test-suffix "Spec"))
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
