;; -*- no-byte-compile: t; -*-
;;; lang/ruby/packages.el

;; Major modes
(package! ruby-mode :built-in t)
(package! yard-mode)

;; REPL
(package! inf-ruby)
(package! company-inf-ruby)

;; Programming environment
(package! rubocop)
(package! robe)

;; Project tools
(package! bundler)
(package! rake)

;; Environment management
(when (modulep! +rbenv)
  (package! rbenv))
(when (modulep! +rvm)
  (package! rvm))
(when (modulep! +chruby)
  (package! chruby))

;; Testing frameworks
(package! rspec-mode)
(package! minitest)

;; Rails
(when (modulep! +rails)
  (package! projectile-rails)
  (package! inflections))
