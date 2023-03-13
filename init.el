;;; init.el -*- lexical-binding: t; -*-

(doom!
 :config default bindings completion disabled ui help
 :app   (rss +org)
 :completion company ivy

 :editor (evil +everywhere) file-templates format
 :editor char-insert fold projects text-manipulation
 :editor snippets word-wrap rotate-text tagging
 ;;:editor multiple-cursors

 :emacs +dired electric ibuffer
 :emacs vc browse misc version-control

 ;; :lang-data csv graphql json julia ledger toml xml yaml

 ;; :lang-dsl  acab-ide ai-and-logic faust fortran music nix qt rest sh solidity

 ;; :lang-strongly-typed agda coq dotnet-langs fstar haskell
 ;; :lang-strongly-typed idris jvm-langs lean ocaml rust +rust scala sml

 ;; :lang-text +org bibtex inform latex markdown plantuml rst web
 :lang-text (org +gnuplot +pomodoro +journal +noter)

 :lang-weakly-typed lisp-langs (python +conda +pyright +poetry +cython)
 ;; :lang-weakly-typed erlang-vms godot lua python ruby

 :lang emacs-lisp
 ;; :lang agda coq csharp data elixir erlang ess
 ;; :lang faust fsharp fstar gdscript (haskell +dante)
 ;; :lang idris json julia kotlin latex lean
 ;; :lang ledger lua markdown nix ocaml plantuml
 ;; :lang qt racket rest rst (ruby +rails) (rust +lsp)
 ;; :lang scala scheme sh sml solidity
 ;; :lang web yaml

 :ui deft doom
 :ui hl-todo hydra indent-guides ligatures
 :ui minimap modeline neotree ophints (popup +defaults)
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
