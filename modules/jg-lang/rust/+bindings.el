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

(map! :after rust-mode
      :map rust-mode-map
      :localleader
      :desc "Docs: Rust"         "1" (cmd! (+jg-misc-browse-url jg-python-docs-url))
      :desc "Docs: Cargo"        "2" (cmd! (+jg-misc-browse-url jg-cargo-docs-url))
      :desc "Docs: Stdlb"        "3" (cmd! (+jg-misc-browse-url jg-rust-stdlib-url))
      :desc "Docs: Editions"     "4" (cmd! (+jg-misc-browse-url jg-rust-edition-url))
      :desc "Docs: Rustdoc"      "5" (cmd! (+jg-misc-browse-url jg-rustdoc-url))
      :desc "Docs: Rust-CPython" "6" (cmd! (+jg-misc-browse-url jg-rust-cpython-url))
      :desc "Docs: Rust Py03"    "7" (cmd! (+jg-misc-browse-url jg-rust-pyo3-url))
      )



;;; +bindings.el ends here
