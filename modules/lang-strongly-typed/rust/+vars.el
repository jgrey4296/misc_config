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

(setq rustic-lsp-client (if (modulep! :tools lsp +eglot) 'eglot 'lsp-mode) ;; HACK `rustic' sets up some things too early. I'd rather disable it and let our respective modules standardize how they're initialized.
      rustic-indent-method-chain t
      rust-prettify-symbols-alist nil ;; Conflicts with (and is redundant with) :ui ligatures
      rustic-babel-format-src-block nil ;; Leave automatic reformatting to the :editor format module.
      rustic-format-trigger nil
      )

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  )

;;-- jg-company
(defvar jg-rust-company-activation (rx (| "stack" "heap" "derive" "cols" "trait" "val")))
(defvar jg-rust-company-kws (let ((ht (make-hash-table :test 'equal)))
                              (puthash "stack"  jg-rust-ivy-stack-types ht)
                              (puthash "heap"   jg-rust-ivy-heap-types ht)
                              (puthash "derive" jg-rust-ivy-derives ht)
                              (puthash "trait"  jg-rust-ivy-traits ht)
                              (puthash "val"    jg-rust-ivy-val ht)
                              ht
                              )
  )


;;-- end jg-company

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
                     ("Makefile\\.toml$" :trigger "__"            :mode cargo-makefile-mode)
                     ("mod\\.rs$"        :trigger "__mod.rs"      :mode rust-mode)
                     ("main\\.rs$"       :trigger "__main.rs"     :mode rust-mode)
                     ("lib\\.rs$"        :trigger "__lib.rs"      :mode rust-mode)
                     ("tests\\.rs"       :trigger "__tests.rs"    :mode rust-mode)
                     ("build\\.rs"       :trigger "__build.rs"    :mode rust-mode)
                     (rustic-mode        :trigger "__"            )
                     (rust-mode          :trigger "__"            )
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
                    )

(spec-handling-add! company
                    '(rust-mode (:front . jg-company/backend) (:front . company-gtags))
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

;;-- end LSP

(spec-handling-add! lookup-regular
                    '((rust-mode)
                      ("rustup" . "https://rust-lang.github.io/rustup/index.html")
                      ("error codes" . "https://doc.rust-lang.org/stable/error_codes/index.html")
                      ("language reference"   . "https://doc.rust-lang.org/stable/reference/index.html")
                      ("Stable Documentation" . "https://doc.rust-lang.org/stable/")
                      ("Md Book"              . "https://rust-lang.github.io/mdBook/")
                      ("Error Codes"          . "https://doc.rust-lang.org/error_codes/error-index.html")
                      ("Decompiler"           . "https://rust.godbolt.org/")
                      ("Rustc"                . "https://doc.rust-lang.org/rustc/index.html")
                      ("Command Line"         . "https://rust-cli.github.io/book/index.html")
                      ("Design Patterns"      . "https://rust-unofficial.github.io/patterns/additional_resources/design-principles.html")
                      ("By Example"           . "https://doc.rust-lang.org/rust-by-example/index.html")
                      ("book"                 . "https://doc.rust-lang.org/stable/book/title-page.html")
                      ("cargo book"           . "https://doc.rust-lang.org/cargo/")
                      ("stdlib"               . "https://doc.rust-lang.org/std/index.html")
                      ("editions"             . "https://doc.rust-lang.org/edition-guide/index.html")
                      ("doc manual"           . "https://doc.rust-lang.org/rustdoc/index.html")
                      ("cypthon"              . "https://dgrunwald.github.io/rust-cpython/doc/cpython/")
                      ("py03"                 . "https://pyo3.rs/v0.16.4/")
                      ("manifest reference"   . "https://doc.rust-lang.org/cargo/reference/manifest.html")
                      ("crates"               . "https://crates.io/")
                      ("Embedded Rust"        . "https://docs.rust-embedded.org/book/")
                      ("Collections"          . "https://doc.rust-lang.org/std/collections/index.html")
                      ("Lists"                . "https://rust-unofficial.github.io/too-many-lists/")
                      ("Unsafe Rust"          . "https://doc.rust-lang.org/nightly/nomicon/")
                      ("bindgen"              . "https://rust-lang.github.io/rust-bindgen/introduction.html")
                      ("porting"              . "https://www.jelmer.uk/port-py-to-rust.html")
                      ("rustup install" . "https://dev.to/xs/setting-up-rust-on-macos-in-a-clean-way-13d1")
                      )
                    )
