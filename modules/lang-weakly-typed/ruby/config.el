;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")
(after! projectile
  (add-to-list 'projectile-project-root-files "Gemfile"))

(use-package! ruby-mode  ; built-in
  :commands ruby-mode
  ;; Other extensions are already registered in `auto-mode-alist' by `ruby-mode'
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :config
  (setq ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-mode-local-vars-hook #'tree-sitter!)

  (after! inf-ruby
    (add-hook 'inf-ruby-mode-hook #'doom-mark-buffer-as-real-h)
    ;; switch to inf-ruby from compile if we detect a breakpoint has been hit
    (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

  ;; so class and module pairs work
  (setq-hook! 'ruby-mode-hook sp-max-pair-length 6)
  )

(use-package! yard-mode
  :commands yard-mode
  :hook ruby-mode
  )

(use-package! robe
  :commands robe-mode
  :after yard-mode
  :init
  (add-hook! 'ruby-mode-hook #'robe-mode)
  :config
  (when (boundp 'read-process-output-max)
    ;; Robe can over saturate IPC, making interacting with it slow/clobbering
    ;; the GC, so increase the amount of data Emacs reads from it at a time.
    (setq-hook! '(robe-mode-hook inf-ruby-mode-hook)
      read-process-output-max (* 1024 1024))
    )
  (add-hook 'robe-mode-hook #'evil-normalize-keymaps)
  )

(use-package! rubocop
  :commands rubocop-mode
  :hook (ruby-mode . rubocop-mode)
  )

;;; Package & Ruby version management

(use-package! rake
  :after ruby-mode
  :init
  (setq rake-cache-file (concat doom-cache-dir "rake.cache"))
  (setq rake-completion-system 'default)
  )

(use-package! bundler
  :after ruby-mode
  )

(use-package! chruby
  :after ruby-mode
  :hook (ruby-mode . chruby-use-corresponding)
  :config
  (setq rspec-use-rvm nil
        rspec-use-chruby t)
  )

;;; Testing frameworks

(use-package! rspec-mode
  :commands rspec-mode
  :init
  (setq rspec-use-spring-when-possible nil)
  (when (modulep! :editor evil)
    (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))
  :config
  (setq rspec-use-rvm (executable-find "rvm"))
  )

(use-package! minitest
  :commands minitest-mode
  :config
  (add-hook 'minitest-mode-hook #'evil-normalize-keymaps)
  )

(use-package! projectile-rails
  :after ruby-mode
  :hook ((ruby-mode inf-ruby-mode projectile-rails-server-mode) . projectile-rails-mode)
  :hook (projectile-rails-server-mode . doom-mark-buffer-as-real-h)
  :hook (projectile-rails-mode . auto-insert-mode)
  :init
  (setq auto-insert-query nil)
  (setq inf-ruby-console-environment "development")
  (when (modulep! :lang web)
    (add-hook 'web-mode-hook #'projectile-rails-mode))
  :config
  (add-hook 'projectile-rails-mode-hook #'evil-normalize-keymaps)
  )
