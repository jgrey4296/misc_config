;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(ruby
                       ("\\.\\(?:a?rb\\|aslsx\\)\\'" . ruby-mode)
                       ( "/\\(?:Brew\\|Fast\\)file\\'" . ruby-mode)
                       ("/\\.rspec\\'" . text-mode)
                      )
                    )

(spec-handling-add! popup
                    '(ruby
                      ("^\\*RuboCop" :select t)
                      ("^\\*\\(rspec-\\)?compilation" :size 0.3 :ttl nil :select t)
                      ("^\\*\\(projectile-\\)?rails" :ttl nil)
                      )
                    )

(defun +jg-ruby-file-in-tap (file)
  (s-contains? "homebrew" file)
  )

(spec-handling-add! file-templates :form 'override
                    `(ruby
                      ("\\.rb$"     :when ,#'+jg-ruby-file-in-tap :trigger "__forumula"   :mode ruby-mode)
                      ("/lib/.+\\.rb$"      :trigger "__module"   :mode ruby-mode :project t)
                      ("\\.gemspec$"        :trigger "__.gemspec" :mode ruby-mode :project t)
                      ("/Gemfile$"          :trigger "__Gemfile"  :mode ruby-mode :project t)
                      ("/Rakefile$"         :trigger "__Rakefile" :mode ruby-mode :project t)

                      ("/spec_helper\\.rb$" :trigger "__helper"   :mode rspec-mode :project t)
                      ("_spec\\.rb$"                              :mode rspec-mode :project t)
                      ("/\\.rspec$"         :trigger "__.rspec"   :mode rspec-mode :project t)
                      )
                    )