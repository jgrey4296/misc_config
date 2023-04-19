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
(spec-handling-add! lookup-url nil
                   ('erlang
                      ("Erlang/OPT" "https://www.erlang.org/doc/search?q=%s")
                      ("Elixir"     "https://hexdocs.pm/elixir/search.html?q=%s")
                      )
                   )

(spec-handling-add! projects nil
                    ('elixir ("mix.exs") :project-file "mix.exs" :compilation-dir nil :configure nil :compile "mix compile" :test "mix test" :install nil :package nil :run nil :test-suffix "_test" :src-dir "lib/")
                    ('rebar ("rebar.config") :project-file "rebar.config" :compilation-dir nil :configure nil :compile "rebar3 compile" :test "rebar3 do eunit,ct" :install nil :package nil :run nil :test-suffix "_SUITE")
  )
;;-- end specs
