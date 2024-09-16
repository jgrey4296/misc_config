;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! flycheck-credo
  :after elixir-mode
  :config
  (flycheck-credo-setup)
  )

(use-package! alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  )

(use-package! alchemist-company
  :when (modulep! :ide company)
  :after elixir-mode
  :commands alchemist-company
  :config
  ;; Alchemist doesn't use hook symbols to add these backends, so we have to use
  ;; the entire closure to get rid of it.
  (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
    (remove-hook 'alchemist-mode-hook fn)
    (remove-hook 'alchemist-iex-mode-hook fn)))

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +extra-config.el ends here
