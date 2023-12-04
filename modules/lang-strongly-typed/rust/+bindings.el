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
(defvar jg-rust-mode-map (make-sparse-keymap))


(map! :map jg-rust-mode-map
      :after rustic
      :n "RET"                      #'rustic-cargo-run
      :n "|"                        #'general-insert-call
      :desc "Racer Describe" :n "?" #'rustic-racer-describe
      :localleader
      (:prefix ("b" . "build")
       :desc "cargo audit"      "a" #'+rust/cargo-audit
       :desc "cargo build"      "b" #'rustic-cargo-build
       :desc "cargo bench"      "B" #'rustic-cargo-bench
       :desc "cargo check"      "c" #'rustic-cargo-check
       :desc "cargo clippy"     "C" #'rustic-cargo-clippy
       :desc "cargo doc"        "d" #'rustic-cargo-build-doc
       :desc "cargo doc --open" "D" #'rustic-cargo-doc
       :desc "cargo fmt"        "f" #'rustic-cargo-fmt
       :desc "cargo new"        "n" #'rustic-cargo-new
       :desc "cargo outdated"   "o" #'rustic-cargo-outdated
       :desc "cargo run"        "r" #'rustic-cargo-run)
      (:prefix ("t" . "cargo test")
       :desc "all"              "a" #'rustic-cargo-test
       :desc "current test"     "t" #'rustic-cargo-current-test)
      )

(map! :map conf-toml-mode-map
      :desc "Dependency ivy" :n  "?" #'+jg-rust-dependency-ivy
      :localleader
      )

(map! :map rust-test-minor-mode-map
      :n "RET" #'rustic-cargo-current-test
      :i "RET" #'evil-ret
      )


(after! rustic
  (setq rustic-mode-map jg-rust-mode-map)
  )

;;; +bindings.el ends here
