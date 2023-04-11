;;; +vars.el -*- lexical-binding: t; -*-

(after! jg-ui-reapply-hook-ready
  (+jg-projects-add-spec 'haskell-stack '(("stack.yaml")             :project-file "stack.yaml"              :compilation-dir nil :configure nil :compile "stack build"                :test "stack build --test"                     :install nil :package nil             :run nil :test-suffix "Spec"))
  (+jg-projects-add-spec 'haskell-cabal '(projectile-cabal-project-p :project-file nil                       :compilation-dir nil :configure nil :compile "cabal build"                :test "cabal test"                             :install nil :package nil             :run "cabal run" :test-suffix "Spec"))
  )
