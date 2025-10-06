;;; +misc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


(use-package! yard-mode ;; For Syntax highlighting
  :commands yard-mode
  :hook ruby-mode
  )

(use-package! robe ;; for code navigation
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

(use-package! rubocop ;; for linting
  :commands rubocop-mode
  :hook (ruby-mode . rubocop-mode)
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
;;; +misc.el ends here
