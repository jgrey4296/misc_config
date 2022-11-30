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
 )

;;-- end urls

;;-- file templates
(after! jg-file-templates
  ;; rust-mode
  (+jg-completion-add-file-templates
   'rust-mode
   '(
     ("config\\.toml$" :trigger "__config.toml" :mode rust-mode)
     ("mod\\.rs$"      :trigger "__mod.rs"      :mode rust-mode)
     ("Cargo\\.toml$"  :trigger "__Cargo.toml"  :mode rust-mode)
     ("main\\.rs$"     :trigger "__main.rs"     :mode rust-mode)
     ("lib\\.rs$"      :trigger "__lib.rs"      :mode rust-mode)
     ("tests\\.rs"     :trigger "__tests.rs"    :mode rust-mode)
     ("build\\.rs"     :trigger "__build.rs"    :mode rust-mode)
     (rustic-mode      :trigger "__"            )
     (rust-mode        :trigger "__"            )
     )
   )
  )
;;-- end file templates

;;-- browse providers
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Rust Stdlib"     "https://doc.rust-lang.org/std/?search=%s")
            '("Rust Crates.io"  "https://crates.io/search?q=%s")
            '("Rust docs.rs"    "https://docs.rs/releases/search?query=%s")
            '("Rust lib.rs"     "https://lib.rs/search?q=%s")
            '("Rust Forums"     "https://users.rust-lang.org/search?q=%s")
            )
  )

;;-- end browse providers


;;; +vars.el ends here
