;;; init.el -*- lexical-binding: t; -*-

(setq force-load-messages nil)
(defvar templates-loc (expand-file-name "~/github/_templates"))

(load! "defer-macro")
(doom!
 :config     default
 :editor     (evil +everywhere)
 :config     bindings disabled help search ui

 :ui         ivy helm hydra ibuffer minibuffer popup

 :editor format text-manipulation
 :editor tagging
 :editor buffer-nav window-nav
 :editor fold undo
 :editor large-files
 :editor autosave

 :ide version-control support librarian snippets workspaces
 :ide minimap diff company debugger

 :lang-data csv graphql dot sql
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

 :tools pdfs
 :tools mail term eval
 :tools processes
 :tools calendar dired calc

 :ui doom-ui
 :ui ophints

 :config linux_os
 )
