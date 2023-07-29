;;; +vars.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 19, 2022
;; Modified: May 19, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/+vars
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  )

;;-- specs
(spec-handling-add! compile-commands
                   '(rust +jg-rust-get-cargo-commands)
                   )

(spec-handling-add! fold
                    `(rust
                      :modes (rust-mode)
                      :priority 25
                      :triggers (:open-all   ,#'hs-show-all
                                :close-all  ,#'hs-hide-all
                                :toggle     ,#'hs-toggle-hiding
                                :open       ,#'hs-show-block
                                :open-rec   nil
                                :close      ,#'hs-hide-block
                                )
                      )
                    )

(spec-handling-add! popup
                    '(rust
                     ("^\\*rustic-compilation" :vslot -1)
                     )
                    )

(spec-handling-add! file-templates
                    '(rust
                     ("config\\.toml$"   :trigger "__config.toml" :mode rust-mode)
                     ("Cargo\\.toml$"    :trigger "__Cargo.toml"  :mode rust-mode)
                     ("mod\\.rs$"        :trigger "__mod.rs"      :mode rust-mode)
                     ("main\\.rs$"       :trigger "__main.rs"     :mode rust-mode)
                     ("lib\\.rs$"        :trigger "__lib.rs"      :mode rust-mode)
                     ("tests\\.rs"       :trigger "__tests.rs"    :mode rust-mode)
                     ("build\\.rs"       :trigger "__build.rs"    :mode rust-mode)
                     (rustic-mode        :trigger "__"            :priority -98)
                     (rust-mode          :trigger "__"            :priority -99)
                     )
                    )

(spec-handling-add! lookup-url
                    '(rust
                     ("Rust Stdlib"      "https://doc.rust-lang.org/std/?search=%s")
                     ("Rust Crates.io"   "https://crates.io/search?q=%s")
                     ("Rust docs"        "https://docs.rs/releases/search?query=%s")
                     ("Rust Docs stdlib" "https://doc.rust-lang.org/std/?search=%s")
                     ("Rust lib.rs"      "https://lib.rs/search?q=%s")
                     ("Rust Forums"      "https://users.rust-lang.org/search?q=%s")
                     )
                    )

(spec-handling-add! projects
                    '(jg-rust ("Cargo.toml")     :project-file "Cargo.toml" :configure nil :test nil :test-dir nil :test-prefix nil :related-files-fn +jg-rust-related-files-fn)
                    '(rust-cargo ("Cargo.toml") :project-file "Cargo.toml" :compilation-dir nil :configure nil :compile "cargo build" :test "cargo test" :install nil :package nil :run "cargo run")
                    )

(spec-handling-add! docsets '(rust-mode "Rust"))

(spec-handling-add! tree-sit-lang
                    '(rust-mode . rust)
                    '(rustic-mode . rust)
                    )

(spec-handling-add! company
                    '(rust-mode (:front jg-company/backend) (:front company-gtags))
                    )

(spec-handling-add! auto-modes
                    '(rust
                      ("\\.rs\\'" . rustic-mode)
                      ("Cargo\\.toml\\'" . conf-toml-mode)
                      ("config\\.toml\\'" .conf-toml-mode)
                      ("\\.h\\'" . c-mode)
                      )
                    )
;;-- end specs

;;-- LSP
(setq lsp-rust-analyzer-server-command '("rustup" "run" "nightly" "rust-analyzer")
      rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer")
      lsp-rust-server 'rust-analyzer
 )

(setq rustic-lsp-client 'lsp-mode
      rustic-indent-method-chain t
      rust-prettify-symbols-alist nil ;; Conflicts with (and is redundant with) :ui ligatures
      rustic-babel-format-src-block nil ;; Leave automatic reformatting to the :editor format module.
      rustic-format-trigger nil
      )
;;-- end LSP

;;-- general-insert
(general-insert-register-processor 'conf-toml-mode "rust-dependencies" #'insert)

;;-- end general-insert
