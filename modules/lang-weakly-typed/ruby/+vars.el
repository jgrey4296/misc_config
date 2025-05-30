;;; +vars.el -*- lexical-binding: t; -*-

(after! rbenv
  (setq rspec-use-rvm nil)
  (add-to-list 'exec-path (expand-file-name "shims" rbenv-installation-dir)))
(after! smartparens
  (sp-local-pair 'ruby-mode "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))
  )

(speckler-add! electric ()
  '(ruby-mode :words '("else" "end" "elsif"))
  )
(speckler-add! repl ()
  '(ruby-mode :start inf-ruby)
  '(robe-mode :start robe-start)
  )
(speckler-add! auto-modes ()
  '(ruby
    ("\\.\\(?:a?rb\\|aslsx\\)\\'" . ruby-mode)
    ( "/\\(?:Brew\\|Fast\\)file\\'" . ruby-mode)
    ("/\\.rspec\\'" . text-mode)
    )
  )
(speckler-add! company ()
  '(ruby-mode (:mode company-robe))
  )
(speckler-add! doc-lookup ()
  `(ruby-mode
    :definition    #'robe-jump
    :documentation #'robe-doc)
  )
(speckler-add! popup ()
  '(ruby
    ("^\\*RuboCop" :select t)
    ("^\\*\\(rspec-\\)?compilation" :size 0.3 :ttl nil :select t)
    ("^\\*\\(projectile-\\)?rails" :ttl nil)
    )
  )
(speckler-add! file-templates ()
  `(ruby
    ("\\.rb$"             :trigger "__forumula"   :mode ruby-mode :when #'+jg-ruby-file-in-tap )
    ("/lib/.+\\.rb$"      :trigger "__module"     :mode ruby-mode :project t)
    ("\\.gemspec$"        :trigger "__.gemspec"   :mode ruby-mode :project t)
    ("/Gemfile$"          :trigger "__Gemfile"    :mode ruby-mode :project t)
    ("/Rakefile$"         :trigger "__Rakefile"   :mode ruby-mode :project t)

    ("/spec_helper\\.rb$" :trigger "__helper"     :mode rspec-mode :project t)
    ("_spec\\.rb$"                                :mode rspec-mode :project t)
    ("/\\.rspec$"         :trigger "__.rspec"     :mode rspec-mode :project t)
    )
  )
