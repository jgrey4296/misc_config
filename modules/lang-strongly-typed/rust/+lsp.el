;;; +lsp.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(advice-add 'rustic-install-lsp-client-p :override #'+rust--dont-install-packages-a)

(use-package! lsp-rust
  :defer t
  )

(use-package! rustic-lsp
  :after rustic
  :defer t
  ;; :init
  ;; (setq rustic-lsp-setup-p nil)
  )

(speckler-setq! rust-lsp ()
  ;; lsp-rust-analyzer-server-command '("rustup" "run" "stable" "rust-analyzer")
  ;; rustic-analyzer-command          '("rustup" "run" "stable" "rust-analyzer")
  lsp-rust-analyzer-server-command '("rust-analyzer")
  rustic-analyzer-command          '("rust-analyzer")
  lsp-rust-server                  'rust-analyzer
  rustic-lsp-client                'lsp-mode

  rustic-cargo-check-exec-command "clippy"

  rust-prettify-symbols-alist   nil ;; Conflicts with (and is redundant with) :ui ligatures
  rustic-babel-format-src-block nil ;; Leave automatic reformatting to the :editor format module.
  rustic-format-trigger         nil
  )

(speckler-add! lib-env ()
  :override t
  '(rs-lsp
    :lang rust
    :setup #'(lambda (state &rest rest) (require 'lsp-mode) (require 'rustic-lsp))
    :start #'(lambda (state &rest rest)
               (add-hook 'rustic-mode-hook #'rustic-setup-lsp)
               )
    :stop  #'(lambda (state &rest rest)
               (remove-hook 'rustic-mode-hook #'rustic-setup-lsp)
               )

    )
)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 19, 2024
;; Modified:   December 19, 2024
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
;;; +lsp.el ends here
