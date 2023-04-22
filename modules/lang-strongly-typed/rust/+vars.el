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


(setq rustic-lsp-client nil ;; HACK `rustic' sets up some things too early. I'd rather disable it and let our respective modules standardize how they're initialized.
      rustic-indent-method-chain t
      rust-prettify-symbols-alist nil ;; Conflicts with (and is redundant with) :ui ligatures
      rustic-babel-format-src-block nil ;; Leave automatic reformatting to the :editor format module.
      rustic-format-trigger nil
      )

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  )


;;-- specs
(spec-handling-add! popup nil
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

(spec-handling-add! lookup-url nil
                    '(rust
                     ("Rust Stdlib"     "https://doc.rust-lang.org/std/?search=%s")
                     ("Rust Crates.io"  "https://crates.io/search?q=%s")
                     ("Rust docs.rs"    "https://docs.rs/releases/search?query=%s")
                     ("Rust lib.rs"     "https://lib.rs/search?q=%s")
                     ("Rust Forums"     "https://users.rust-lang.org/search?q=%s")
                     )
                    )

(defun +jg-rust-related-files-fn (path)
    " Given a relative path to a file, provide projectile with various :kinds of related file "
    (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
          (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
          ;;(init-file  (f-join (f-parent path) "__init__.py"))
          (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
          ;;(error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
          (project    (f-join (projectile-project-root) "Cargo.toml"))
          (is-test (s-matches? "^test_" (f-filename path)))
          )
      (append (when is-test (list :impl impl-file))
              (unless is-test (list :test test-file))
              (when (s-matches? "\/cli\/" path) (list :project project))
              (list :init-py init-file)
              (list :log log-file)
              (list :errors error-file)
              )
      )
    )

(spec-handling-add! projects t
                    '(jg-rust ("Cargo.toml")     :project-file "Cargo.toml" :configure nil :test nil :test-dir nil :test-prefix nil :related-files-fn +jg-rust-related-files-fn)
                    '(rust-cargo ("Cargo.toml") :project-file "Cargo.toml" :compilation-dir nil :configure nil :compile "cargo build" :test "cargo test" :install nil :package nil :run "cargo run")
                    )

(spec-handling-add! lookup-regular nil
                    '((rust-mode rustic-mode)
                      ("Rust By Example"         . "https://doc.rust-lang.org/rust-by-example/index.html")
                      ("rust book"               . "https://doc.rust-lang.org/stable/book/title-page.html")
                      ("cargo book"              . "https://doc.rust-lang.org/cargo/")
                      ("rust stdlib"             . "https://doc.rust-lang.org/std/index.html")
                      ("rust editions"           . "https://doc.rust-lang.org/edition-guide/index.html")
                      ("rust doc manual"         . "https://doc.rust-lang.org/rustdoc/index.html")
                      ("rust cypthon"            . "https://dgrunwald.github.io/rust-cpython/doc/cpython/")
                      ("rust py03"               . "https://pyo3.rs/v0.16.4/")
                      ("rust manifest reference" . "https://doc.rust-lang.org/cargo/reference/manifest.html")
                      ("rust crates"             . "https://crates.io/")
                      ("Embedded Rust"           . "https://docs.rust-embedded.org/book/")
                     )
                    )
 (set-docsets! 'rustic-mode "Rust")
;;-- end specs

;;-- LSP
(setq lsp-rust-analyzer-server-command '("rustup" "run" "nightly" "rust-analyzer")
      rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer")
      lsp-rust-server 'rust-analyzer
 )


;;-- end LSP
