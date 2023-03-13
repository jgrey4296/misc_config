;;; init.el -*- lexical-binding:          t; -*-

(doom!
 :config default bindings completion disabled ui
 :app   (rss +org)
 :completion company ivy

 :editor (evil +everywhere) file-templates format
 char-insert fold projects text-manipulation
 snippets word-wrap rotate-text tagging
 ;;multiple-cursors

 :emacs (+dired +dirvish) electric ibuffer
 vc browse misc version-control


 :lang-data
 ;; csv graphql json julia ledger toml xml yaml
 :lang-dsl
 ;; acab-ide ai-and-logic faust fortran music nix qt rest sh solidity
 :lang-strongly-typed
 ;; agda coq dotnet-langs fstar haskell idris jvm-langs lean ocaml rust +rust scala sml
 :lang-text
 ;; +org bibtex inform latex markdown plantuml rst web
 (org +gnuplot +pomodoro +journal +noter)
 :lang-weakly-typed
 ;; erlang-vms godot lisp-langs lua python ruby
 (python +conda +pyright +poetry +cython)

 :ui deft doom
 hl-todo hydra indent-guides ligatures
 minimap modeline neotree ophints (popup +defaults)
 vc-gutter vi-tilde-fringe window-select workspaces
 ;;treemacs unicode (emoji +unicode)

 :term shell term vterm
 ;;eshell

 :checkers syntax (spell +flyspell) grammar

 :tools biblio debugger editorconfig (eval +overlay)
 lookup lsp magit rgb tmux tree-sitter mail twitter
 ;; ein make

 :email (mu4e +gmail)

 :os (:if IS-MAC macos) tty

 :lang
 emacs-lisp
 ;; agda coq csharp data elixir erlang ess
 ;; faust fsharp fstar gdscript (haskell +dante)
 ;; idris json julia kotlin latex lean
 ;; ledger lua markdown nix ocaml plantuml
 ;; qt racket rest rst (ruby +rails) (rust +lsp)
 ;; scala scheme sh sml solidity
 ;; web yaml
 )
