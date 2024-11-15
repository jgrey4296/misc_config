;;; compile.el -*- lexical-binding: t; -*-
(require 'dash)

;;;###autoload
(defun +jg-rust-get-cargo-commands (&optional dir)
  (interactive)
  (-when-let*  ((root (projectile-project-root dir))
                (cargo (f-join root "Cargo.toml"))
                (cargo-exists (f-exists? cargo))
                )
    (+jg-projects-annotate-cmds
     '("build       Compile the current crate"
       "check       Report crate errors"
       "clean       Remove the crate build directory"
       "doc         Build this package's and its dependencies' documentation"
       "new         Create a new cargo crate"
       "init        Create a new cargo module in an existing directory"
       "add         Add dependencies to a manifest file"
       "remove      Remove dependencies from a manifest file"
       "run         Run a binary or example of the local package"
       "test        Run the tests"
       "bench       Run the benchmarks"
       "update      Update dependencies listed in Cargo.lock"
       "search      Search registry for crates"
       "publish     Package and upload this package to the registry"
       "install     Install a Rust binary. Default location is $HOME/.cargo/bin"
       "uninstall   Uninstall a Rust binary"
       )
     #'(lambda (x) (concat "cargo" " " (car (split-string x" " t " "))))
    )
    )
  )
