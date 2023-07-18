;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map robe-mode-map
      :localleader
      "'"  #'robe-start
      "h"  #'robe-doc
      "R"  #'robe-rails-refresh
      :prefix "s"
      "d"  #'ruby-send-definition
      "D"  #'ruby-send-definition-and-go
      "r"  #'ruby-send-region
      "R"  #'ruby-send-region-and-go
      "i"  #'ruby-switch-to-inf
      )

(map! :map ruby-mode-map
      :localleader
      "[" #'ruby-toggle-block
      "{" #'ruby-toggle-block
      (:prefix ("k" . "rake")
               "k" #'rake
               "r" #'rake-rerun
               "R" #'rake-regenerate-cache
               "f" #'rake-find-task)
      ( :prefix ("b" . "bundle")
                "c" #'bundle-check
                "C" #'bundle-console
                "i" #'bundle-install
                "u" #'bundle-update
                "e" #'bundle-exec
                "o" #'bundle-open)
      )

(map! :map rubocop-mode-map
      :localleader
      "f" #'rubocop-check-current-file
      "F" #'rubocop-autocorrect-current-file
      "p" #'rubocop-check-project
      "P" #'rubocop-autocorrect-project
      )

(map! :localleader
      :prefix "t"
      :map (rspec-verifiable-mode-map rspec-dired-mode-map rspec-mode-map)
      "a" #'rspec-verify-all
      "r" #'rspec-rerun
      :map (rspec-verifiable-mode-map rspec-mode-map)
      "v" #'rspec-verify
      "c" #'rspec-verify-continue
      "l" #'rspec-run-last-failed
      "T" #'rspec-toggle-spec-and-target
      "t" #'rspec-toggle-spec-and-target-find-example
      :map rspec-verifiable-mode-map
      "f" #'rspec-verify-method
      "m" #'rspec-verify-matching
      :map rspec-mode-map
      "s" #'rspec-verify-single
      "e" #'rspec-toggle-example-pendingness
      :map rspec-dired-mode-map
      "v" #'rspec-dired-verify
      "s" #'rspec-dired-verify-single
      )

(map! :map minitest-mode-map
      :localleader
      :prefix "t"
      "r" #'minitest-rerun
      "a" #'minitest-verify-all
      "s" #'minitest-verify-single
      "v" #'minitest-verify)

(map! :localleader
        :map projectile-rails-mode-map
        "r" #'projectile-rails-command-map
        )
