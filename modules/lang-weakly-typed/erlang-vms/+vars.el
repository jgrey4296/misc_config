;;; +vars.el -*- lexical-binding: t; -*-

(setq inferior-erlang-buffer-name "*erlang*"
      erlang-shell-function 'inferior-erlang

      alchemist-iex-program-name "iex"
      elixir-shell-buffer-name "*Alchemist-IEx*"
      )

;;-- browse providers
(after! jg-ui-reapply-hook-ready
  (+jg-browse-add-lookup-spec 'erlang
            '(
              ("Erlang/OPT" "https://www.erlang.org/doc/search?q=%s")
              ("Elixir"     "https://hexdocs.pm/elixir/search.html?q=%s")
            )
            )
  )

;;-- end browse providers

;;-- links
(setq elixir-eex-url      "https://hexdocs.pm/eex/EEx.html"
      elixir-kernal-url   "https://hexdocs.pm/elixir/Kernel.html"
      elixir-unittest-url "https://hexdocs.pm/ex_unit/ExUnit.html"
      elixir-repl-url     "https://hexdocs.pm/iex/IEx.html"
      elixir-logger-url   "https://hexdocs.pm/logger/Logger.html"
      elixir-mix-url      "https://hexdocs.pm/mix/Mix.html"

      )
