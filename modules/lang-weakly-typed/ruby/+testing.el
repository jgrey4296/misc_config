;;; +testing.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


;;; Testing frameworks

(use-package! rspec-mode ;; BDD/TDD
  :commands rspec-mode
  :init
  (setq rspec-use-spring-when-possible nil)
  (when (modulep! :editor evil)
    (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))
  :config
  (setq rspec-use-rvm (executable-find "rvm"))
  )

(use-package! minitest ;; testing
  :commands minitest-mode
  :config
  (add-hook 'minitest-mode-hook #'evil-normalize-keymaps)
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
;;; +testing.el ends here
