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
(spec-handling-add! auto-modes
                    '(erlang
                      ("\\.erlang\\'"                          . erlang-mode)
                      ("/rebar\\.config\\(?:\\.script\\)?\\'"  . erlang-mode)
                      ("/\\(?:app\\|sys\\)\\.config\\'"        . erlang-mode)
                      ("\\.elixir\\'"                          . elixir-mode)
                      ("\\.ex\\'"                              . elixir-mode)
                      ("\\.exs\\'"                             . elixir-mode)
                      ("mix\\.lock"                            . elixir-mode)
                      )
                    )
(spec-handling-add! lookup-handler
                    `(elixir-mode
                     :definition    ,#'alchemist-goto-definition-at-point
                     :documentation ,#'alchemist-help-search-at-point
                     )
                    )
(spec-handling-add! eval
                    `(elixir-mode
                      :start ,#'alchemist-iex-project-run
                      :send ,#'alchemist-eval-region
                      )
                    `(erlang-mode :start ,#'+erlang/open-repl)
                    )
(spec-handling-add! ligatures
                    '(elixir-mode
                      ;; Functional
                      :def "def"
                      :lambda "fn"
                      ;; :src_block "do"
                      ;; :src_block_end "end"
                      ;; Flow
                      :not "!"
                      :in "in" :not-in "not in"
                      :and "and" :or "or"
                      :for "for"
                      :return "return" :yield "use"
                      )
                    )

(spec-handling-add! file-templates
                    '(elixir
                      '("\\.ex\\'"     :trigger "__" :mode elixir-mode :priority -99)
                      '("\\.exs\\'"    :trigger "__" :mode elixir-mode :priority -99)
                      '("\\.elixir\\'" :trigger "__" :mode elixir-mode :priority -99)
                      )
                    )
;;-- end specs
