;;; init.el -*- lexical-binding: t; -*-

(doom!
 :config default bindings completion disabled help programming search snippets ui
 :app   (rss +org)
 :completion company ivy

 :editor char-insert fold navigation projects tagging text-manipulation
 :editor (evil +everywhere) format
 :editor fold snippets word-wrap rotate-text
 ;;:editor multiple-cursors

 :emacs electric vc
 :emacs browse dired hydra ibuffer misc popup version-control

 ;; :lang-data csv graphql
 :lang-data xml toml json yaml logs

 ;; :lang-dsl cobol fortran solidity
 :lang-dsl acab-ide ai-and-logic
 :lang-dsl rest sh nix
 :lang-dsl qt
 ;; :lang-dsl music faust

 ;; :lang-strongly-typed fstar haskell
 ;; :lang-strongly-typed idris lean ocaml scala sml
 :lang-strongly-typed coq rust +rust
 :lang-strongly-typed jvm-langs dotnet-langs

 :lang-text inform web
 :lang-text (org +gnuplot +pomodoro +journal +noter) +org
 :lang-text bibtex latex markdown plantuml rst

 :lang-weakly-typed lisp-langs (python +conda +pyright +cython +lsp) lua
 ;; :lang-weakly-typed erlang-vms godot ruby

 :lang emacs-lisp ledger
 ;; :lang agda csharp elixir erlang ess
 ;; :lang fsharp gdscript
 ;; :lang julia kotlin
 ;; :lang racket rst
 ;; :lang scheme

 :ui deft doom
 :ui hl-todo indent-guides ligatures
 :ui minimap modeline neotree ophints
 :ui vc-gutter vi-tilde-fringe window-select workspaces
 ;;:ui treemacs unicode (emoji +unicode)

 :term shell term vterm
 ;;:term eshell

 :checkers syntax (spell +flyspell) grammar

 :tools debugger doot lookup (eval +overlay) lsp magit mail tmux twitter pdfs
 :tools editorconfig rgb tree-sitter mail
 ;;:tools ein make

 :email (mu4e +gmail)

 :os (:if IS-MAC macos) tty

 )
