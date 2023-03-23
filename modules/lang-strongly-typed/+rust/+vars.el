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
;;-- urls
(setq-default jg-rust-docs-url         "https://doc.rust-lang.org/stable/book/title-page.html"
              jg-cargo-docs-url        "https://doc.rust-lang.org/cargo/"
              jg-rust-stdlib-url       "https://doc.rust-lang.org/std/index.html"
              jg-rust-edition-url      "https://doc.rust-lang.org/edition-guide/index.html"
              jg-rustdoc-url           "https://doc.rust-lang.org/rustdoc/index.html"
              jg-rust-cpython-url      "https://dgrunwald.github.io/rust-cpython/doc/cpython/"
              jg-rust-pyo3-url         "https://pyo3.rs/v0.16.4/"
              jg-rust-cargo-toml-url   "https://doc.rust-lang.org/cargo/reference/manifest.html"
              jg-rust-cargo-search-url "https://crates.io/search?q=%s"
              jg-rust-cargo-make-url   "https://sagiegurari.github.io/cargo-make/"
              jg-rust-by-example-url   "https://doc.rust-lang.org/stable/rust-by-example/index.html"
 )

;;-- end urls

;;-- file spec
(after! jg-ui-reapply-hook-ready
  ;; rust-mode
  (+jg-snippets-add-file-spec 'rust-mode
                              '(
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
  )
;;-- end file templates

;;-- browse spec
(after! jg-ui-reapply-hook-ready
  (+jg-browse-add-lookup-spec 'rust
                              '(("Rust Stdlib"     "https://doc.rust-lang.org/std/?search=%s")
                                ("Rust Crates.io"  "https://crates.io/search?q=%s")
                                ("Rust docs.rs"    "https://docs.rs/releases/search?query=%s")
                                ("Rust lib.rs"     "https://lib.rs/search?q=%s")
                                ("Rust Forums"     "https://users.rust-lang.org/search?q=%s")
                                )
                              )
  )

;;-- end browse providers

;;-- LSP
(setq lsp-rust-analyzer-server-command '("rustup" "run" "nightly" "rust-analyzer")
      rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer")
      lsp-rust-server 'rust-analyzer
 )


;;-- end LSP
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
(after! jg-ui-reapply-hook-ready
  (+jg-projects-add-spec 'jg-rust-project '(("Cargo.toml") :project-file "Cargo.toml" :configure   nil :test        nil :test-dir    nil :test-prefix nil :related-files-fn #'+jg-rust-related-files-fn))
  )
;;; +vars.el ends here
