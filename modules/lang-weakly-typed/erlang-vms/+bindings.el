;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map erlang-mode-map
      :localleader


      )

(map! :map elixir-mode-map

      :localleader

      )

(map! :after elixir-mode
        :localleader
        :map elixir-mode-map
        :prefix ("t" . "test")
        "a" #'exunit-verify-all
        "r" #'exunit-rerun
        "v" #'exunit-verify
        "T" #'exunit-toggle-file-and-test
        "t" #'exunit-toggle-file-and-test-other-window
        "s" #'exunit-verify-single)

(map! :after elixir-mode
        :localleader
        :map elixir-mode-map
        "m" #'alchemist-mix
        "c" #'alchemist-mix-compile
        "i" #'alchemist-iex-project-run
        "f" #'elixir-format
        (:prefix ("e" . "eval")
         "e" #'alchemist-iex-send-last-sexp
         "r" #'alchemist-iex-send-region
         "l" #'alchemist-iex-send-current-line
         "R" #'alchemist-iex-reload-module))
