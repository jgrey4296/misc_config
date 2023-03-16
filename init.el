;;; init.el -*- lexical-binding: t; -*-

(doom!
 :config default bindings completion disabled ui help
 :app   (rss +org)
 :completion company ivy

 :editor (evil +everywhere) file-templates format
 :editor char-insert fold projects text-manipulation
 :editor snippets word-wrap rotate-text tagging
 ;;:editor multiple-cursors

 :emacs +dired electric
 :emacs vc browse misc version-control

 ;; :lang-data csv graphql
 :lang-data xml toml json yaml logs

 ;; :lang-dsl cobol fortran solidity
 :lang-dsl acab-ide ai-and-logic
 :lang-dsl rest sh nix
 :lang-dsl qt
 ;; :lang-dsl music faust

 ;; :lang-strongly-typed dotnet-langs fstar haskell
 ;; :lang-strongly-typed idris jvm-langs lean ocaml scala sml
 :lang-strongly-typed coq rust +rust

 :lang-text inform web
 :lang-text (org +gnuplot +pomodoro +journal +noter) +org
 :lang-text bibtex latex markdown plantuml rst

 :lang-weakly-typed lisp-langs (python +conda +pyright +poetry +cython +lsp)
 ;; :lang-weakly-typed erlang-vms godot lua python ruby

 :lang emacs-lisp ledger
 ;; :lang agda csharp elixir erlang ess
 ;; :lang fsharp gdscript
 ;; :lang julia kotlin
 ;; :lang racket rst
 ;; :lang scheme

 :ui deft doom ibuffer hydra popup
 :ui hl-todo indent-guides ligatures
 :ui minimap modeline neotree ophints
 :ui vc-gutter vi-tilde-fringe window-select workspaces
 ;;:ui treemacs unicode (emoji +unicode)

 :term shell term vterm
 ;;:term eshell

 :checkers syntax (spell +flyspell) grammar

 :tools biblio debugger editorconfig (eval +overlay)
 :tools lookup lsp magit rgb tmux tree-sitter mail twitter
 ;;:tools ein make

 :email (mu4e +gmail)

 :os (:if IS-MAC macos) tty

 )
