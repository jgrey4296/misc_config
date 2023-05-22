;;; init.el -*- lexical-binding: t; -*-

(doom!
 :config default bindings disabled help search ui
 :app   (rss +org)
 :completion company ivy helm

 :editor (evil +everywhere) format
 :editor char-insert fold navigation tagging text-manipulation
 :editor fold snippets lookup workspaces

 :emacs electric vc autosave
 :emacs dired hydra ibuffer popup version-control

 :lang-data csv graphql
 :lang-data xml toml json yaml logs

 ;; :lang-dsl cobol fortran solidity
 :lang-dsl acab-ide ai-and-logic
 :lang-dsl rest sh nix
 :lang-dsl qt
 ;; :lang-dsl music faust

 :lang-strongly-typed fstar haskell
 :lang-strongly-typed idris lean
 :lang-strongly-typed coq rust
 :lang-strongly-typed jvm-langs dotnet-langs ml-langs

 :lang-text inform web
 :lang-text (org +gnuplot +pomodoro +journal +noter)
 :lang-text bibtex latex markdown plantuml rst

 :lang-weakly-typed lisp-langs (python +conda +pyright +cython +lsp) lua
 :lang-weakly-typed erlang-vms godot ruby

 :lang emacs-lisp ledger
 ;; :lang agda ess
 ;; :lang julia

 :ui deft doom
 :ui minimap neotree ophints
 ;;:ui treemacs unicode (emoji +unicode)

 :checkers syntax (spell +flyspell) grammar

 :tools (eval +overlay) lsp mail twitter pdfs
 :tools editorconfig rgb tree-sitter mail term
 :tools taskrunners
 ;;:tools ein make debugger

 :email (mu4e +gmail)

 :os (:if IS-MAC macos) tty

 )
