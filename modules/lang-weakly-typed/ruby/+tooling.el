;;; +tooling.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! rake ;; ruby's make
  :after ruby-mode
  :init
  (setq rake-cache-file (concat doom-cache-dir "rake.cache"))
  (setq rake-completion-system 'default)
  )

(use-package! bundler
  :after ruby-mode
  )

(use-package! chruby ;; change ruby versions
  :after ruby-mode
  :hook (ruby-mode . chruby-use-corresponding)
  :config
  (setq rspec-use-rvm nil
        rspec-use-chruby t)
  )

(use-package! projectile-rails ;;
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

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 20, 2025
;; Modified:   September 20, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +tooling.el ends here
