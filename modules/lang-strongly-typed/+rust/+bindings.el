;;; +bindings.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 19, 2022
;; Modified: May 19, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/+bindings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(map! :after rustic
      :map rustic-mode-map
      :localleader
      :prefix ("d" . "Docs")
      :desc "Rust"         "1" (cmd! (browse-url jg-rust-docs-url))
      :desc "Cargo"        "2" (cmd! (browse-url jg-cargo-docs-url))
      :desc "Stdlb"        "3" (cmd! (browse-url jg-rust-stdlib-url))
      :desc "Editions"     "4" (cmd! (browse-url jg-rust-edition-url))
      :desc "Rustdoc"      "5" (cmd! (browse-url jg-rustdoc-url))
      :desc "Rust-CPython" "6" (cmd! (browse-url jg-rust-cpython-url))
      :desc "Rust Py03"    "7" (cmd! (browse-url jg-rust-pyo3-url))
      :desc "Rust by example" "8" (cmd! (browse-rul "https://doc.rust-lang.org/rust-by-example/index.html"))
      )

(map! :map conf-toml-mode-map
      :desc "Dependency ivy" :n  "?" #'+jg-rust-dependency-ivy
      :localleader
      :desc "Cargo reference"    "2" (cmd! (browse-url jg-rust-cargo-toml-url))
      :desc "Cargo-Make"         "3" (cmd! (browse-url jg-rust-cargo-make-url))
      )

(after! rustic
  (+jg-bindings-undefine-metas rust-mode-map)
  (+jg-bindings-undefine-metas rustic-mode-map)
  )

;;; +bindings.el ends here