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
(speckler-add! compile-commands ()
  '(rust +jg-rust-get-cargo-commands)
  )

(speckler-add! fold ()
  `(rust
    :modes (rust-mode)
    :priority 25
    :triggers (:open-all   #'hs-show-all
               :close-all  #'hs-hide-all
               :toggle     #'hs-toggle-hiding
               :open       #'hs-show-block
               :open-rec   nil
               :close      #'hs-hide-block
               )
    )
  )

(speckler-add! popup ()
  '(rust
    ("^\\*rustic-compilation" :vslot -1)
    )
  )

(speckler-add! file-templates ()
  '(rust
    ("config\\.toml$"   :trigger "__rust_config"      :mode conf-toml-mode)
    ("Cargo\\.toml$"    :trigger "__cargo"            :mode conf-toml-mode)
    ("mod\\.rs$"        :trigger "__mod.rs"           :mode rust-mode)
    ("main\\.rs$"       :trigger "__main.rs"          :mode rust-mode)
    ("lib\\.rs$"        :trigger "__lib.rs"           :mode rust-mode)
    ("tests\\.rs"       :trigger "__tests.rs"         :mode rust-mode)
    ("build\\.rs"       :trigger "__build.rs"         :mode rust-mode)
    (rustic-mode        :trigger "__"            :priority -98)
    (rust-mode          :trigger "__"            :priority -99)
    )
  )

(speckler-add! lookup-url ()
  '(rust
    ("Rust Stdlib"      "https://doc.rust-lang.org/std/?search=%s")
    ("Rust Crates.io"   "https://crates.io/search?q=%s")
    ("Rust docs"        "https://docs.rs/releases/search?query=%s")
    ("Rust Docs stdlib" "https://doc.rust-lang.org/std/?search=%s")
    ("Rust lib.rs"      "https://lib.rs/search?q=%s")
    ("Rust Forums"      "https://users.rust-lang.org/search?q=%s")
    )
  )

(speckler-add! projects ()
  '(jg-rust ("Cargo.toml")     :project-file "Cargo.toml" :test nil :test-dir nil :test-prefix nil :related-files-fn +jg-rust-related-files-fn)
  '(rust-cargo ("Cargo.toml")  :project-file "Cargo.toml" :compilation-dir nil :configure nil :compile "cargo build" :test "cargo test" :install nil :package nil :run "cargo run")
  )

(speckler-add! docsets ()
  '(rust-mode "Rust")
  )

(speckler-add! tree-sit-lang ()
  '(rust-mode . rust)
  '(rustic-mode . rust)
  )

(speckler-add! company ()
  '(rust-mode (:front jg-company/backend) (:front company-gtags))
  )

(speckler-add! auto-modes ()
  '(rust
    ("\\.rs\\'" . rustic-mode)
    ("Cargo\\.toml\\'" . conf-toml-mode)
    ("config\\.toml\\'" .conf-toml-mode)
    ("\\.h\\'" . c-mode)
    )
  )

(speckler-add! babel ()
  '(rust
    (:name rust       :lib rustic-babel)
    )
  )

(speckler-add! org-src ()
  '(rust
    ("rust" . rustic)
    )
  )
;;-- end specs

;;-- general-insert
(librarian-insert-register-processor 'conf-toml-mode "rust-dependencies" #'insert)

;;-- end general-insert
