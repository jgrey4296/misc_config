;;; init.el -*- lexical-binding: t; -*-


(load! "defer-macro")
(doom!
 :config default
 :editor (evil +everywhere)
 :config bindings disabled help search ui
 :completion company ivy helm

 :editor format
 :editor char-insert fold navigation tagging text-manipulation
 :editor fold snippets lookup workspaces diff undo
 :editor large-files

 :emacs autosave calendar minibuffer
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

 :checkers (spell +flyspell)

 :tools ide-support twitter pdfs
 :tools mail term
 :tools taskrunners
 ;;:tools debugger

 ;; --------------------------------------------------
 ;; Doom Standard modules
 :app   (rss +org)
 :email (mu4e +gmail)
 :emacs electric vc
 :lang ledger
 ;; :lang agda ess
 ;; :lang julia
 :ui deft doom
 :ui minimap ophints
 ;;:ui treemacs unicode (emoji +unicode)
 ;; :tools make
 :tools (eval +overlay)
 :tools editorconfig rgb
 :os (:if IS-MAC macos) tty
 )
