;;; +vars.el -*- lexical-binding: t; -*-

;;-- projectile
(after! jg-ui-reapply-hook-ready
  (+jg-projects-add-spec 'rust-cargo '(("Cargo.toml") :project-file "Cargo.toml" :compilation-dir nil :configure nil :compile "cargo build" :test "cargo test" :install nil :package nil :run "cargo run"))

  )
;;-- end projectile
