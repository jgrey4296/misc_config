;;; init.el -*- lexical-binding: t; -*-

(setq force-load-messages nil)

(load! "defer-macro")
(doom!
 :config default
 :editor (evil +everywhere)
 :config bindings disabled help search ui
 :completion company ivy helm

 ;; TODO :experimentation

 :editor format
 :editor char-insert fold navigation tagging text-manipulation
 :editor fold snippets lookup workspaces diff undo
 :editor large-files

 :emacs autosave calendar minibuffer
 :emacs dired hydra ibuffer popup version-control

 :lang-data csv graphql dot
 :lang-data xml toml json yaml logs

 :lang-dsl acab ai-and-logic
 :lang-dsl rest sh nix
 :lang-dsl qt
 ;; :lang-dsl music

 :lang-strongly-typed fstar haskell
 :lang-strongly-typed idris lean
 :lang-strongly-typed coq rust
 :lang-strongly-typed jvm-langs dotnet-langs ml-langs

 :lang-text inform web
 :lang-text (org +gnuplot +pomodoro +journal +noter)
 :lang-text bibtex latex markdown plantuml rst

 :lang-weakly-typed lisp-langs python lua
 :lang-weakly-typed erlang-vms godot ruby

 :checkers (spell +flyspell)

 :tools ide-support twitter pdfs
 :tools mail term eval
 :tools  processes

 ;; --------------------------------------------------
 ;; Doom Standard modules
 :app   (rss +org)
 :emacs electric
 :ui deft doom
 :ui minimap ophints
 ;; :tools editorconfig
 :tools rgb
 :os (:if IS-MAC macos) tty
 )
