;;; init.el -*- lexical-binding: t; -*-

(setq force-load-messages nil)

(load! "defer-macro")
(doom!
 :config     default
 :editor     (evil +everywhere)
 :config     bindings disabled help search ui

 :ui         company ivy helm hydra ibuffer minibuffer popup

 ;; TODO :experimentation

 :editor format text-manipulation
 :editor char-insert tagging
 :editor buffer-nav window-nav
 :editor fold undo
 :editor large-files
 :editor autosave


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


 :ide version-control support lookup snippets workspaces diff

 :tools twitter pdfs
 :tools mail term eval
 :tools processes
 :tools (spelling +flyspell)
 :tools calendar dired

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
