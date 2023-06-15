;;; +vars.el -*- lexical-binding: t; -*-

(setq inferior-erlang-buffer-name "*erlang*"
      erlang-shell-function 'inferior-erlang

      alchemist-iex-program-name "iex"
      elixir-shell-buffer-name "*Alchemist-IEx*"
      )

;;-- links
(setq elixir-eex-url      "https://hexdocs.pm/eex/EEx.html"
      elixir-kernal-url   "https://hexdocs.pm/elixir/Kernel.html"
      elixir-unittest-url "https://hexdocs.pm/ex_unit/ExUnit.html"
      elixir-repl-url     "https://hexdocs.pm/iex/IEx.html"
      elixir-logger-url   "https://hexdocs.pm/logger/Logger.html"
      elixir-mix-url      "https://hexdocs.pm/mix/Mix.html"

      )

;;-- end links

;;-- specs
(spec-handling-add! lookup-url
                    '(erlang
                      ("Erlang/OPT" "https://www.erlang.org/doc/search?q=%s")
                      ("Elixir"     "https://hexdocs.pm/elixir/search.html?q=%s")
                      )
                    )
(spec-handling-add! projects
                    '(elixir ("mix.exs") :project-file "mix.exs" :compilation-dir nil :configure nil :compile "mix compile" :test "mix test" :install nil :package nil :run nil :test-suffix "_test" :src-dir "lib/")
                    '(rebar ("rebar.config") :project-file "rebar.config" :compilation-dir nil :configure nil :compile "rebar3 compile" :test "rebar3 do eunit,ct" :install nil :package nil :run nil :test-suffix "_SUITE")
                    )
(spec-handling-add! tree-sit-lang
                    '(elixir-mode     . elixir)
                    )

(spec-handling-add! lookup-handler
                    `(elixir-mode
                     :definition    ,#'alchemist-goto-definition-at-point
                     :documentation ,#'alchemist-help-search-at-point
                     )
                    )
(set-eval-handler! 'elixir-mode #'alchemist-eval-region)
(set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)
;;-- end specs

(spec-handling-add! lookup-regular
                    '(erlang
                      ("Erlang/OPT" . "https://www.erlang.org/doc/")
                      ("Erlang Reference" . "https://www.erlang.org/doc/reference_manual/users_guide.html")
                      ("Erlang system principles" . "https://www.erlang.org/doc/system_principles/system_principles.html")
                      ("Erlang design principles" . "https://www.erlang.org/doc/design_principles/users_guide.html")
                      ("Elixir"     . "https://hexdocs.pm/elixir/")
                      ("Elixir Stdlib" . "https://hexdocs.pm/elixir/Kernel.html")
                      ("Exlir string templates" . "https://hexdocs.pm/eex/EEx.html")
                      ("Elixir Unit test" . "https://hexdocs.pm/ex_unit/ExUnit.html")
                      ("Elixir shell" . "https://hexdocs.pm/iex/IEx.html")
                      ("Elixir logging" . "https://hexdocs.pm/logger/Logger.html")
                      ("Elxir mix build tool" . "https://hexdocs.pm/mix/Mix.html")
                      ("Erlang Nifs" . "https://www.erlang.org/doc/tutorial/nif.html")
                      )
                    )
