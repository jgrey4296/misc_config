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
                      ("^\\*\\(rspec-\\)?compilation" :size 0.3 :ttl nil :select t))
                    ("^\\*\\(projectile-\\)?rails" :ttl nil)
                      )
                    )
