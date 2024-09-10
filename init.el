;;; init.el -*- lexical-binding: t; -*-

(setq force-load-messages nil)
(defvar templates-loc (expand-file-name "~/github/_templates"))

(load! "defer-macro")
(doom!
 :config     default ;; +smartparens
 :editor     (evil +everywhere)
 :config     bindings disabled help search
 :config     ui ;; +light

 :experimentation vertico
 :ui (ivy) ;; +prescient +fuzzy
 :ui helm hydra ibuffer minibuffer
 :ui popup ;; +poppy

 :editor format
 :editor text-manipulation ;; +onsave
 :editor tagging
 :editor buffer-nav
 :editor window-nav ;; +numbers +ace
 :editor fold undo
 :editor large-files
 :editor autosave

 :ide version-control ;; +forge +diff-hl
 :ide librarian snippets workspaces
 :ide (support +lsp +flycheck +tree-sitter) ;; +eglot  +semantic
 :ide minimap diff company debugger environments

 :lang-data csv dot sql nu
 :lang-data graphql
 :lang-data xml toml json yaml logs

 :lang-dsl acab ai-and-logic
 :lang-dsl rest sh nix
 :lang-dsl qt
 ;; :lang-dsl music

 :lang-strongly-typed haskell
 :lang-strongly-typed (proofs) ;; coq, idris, agda, fstar, lean)
 :lang-strongly-typed rust lean
 :lang-strongly-typed jvm-langs dotnet-langs ml-langs

 :lang-text inform web
 :lang-text org ;; +unittest
 :lang-text bibtex latex markdown plantuml rst

 :lang-weakly-typed lisp-langs
 :lang-weakly-typed lua ;; +moonscript
 :lang-weakly-typed (python +isort) ;; +tox
 :lang-weakly-typed erlang-vms godot
 :lang-weakly-typed ruby ;; +rbenv +rvm +chruby

 :tools pdfs
 :tools mail term eval
 :tools processes
 :tools calendar calc
 :tools dired ;; +dirvish

 :ui doom-ui
 :ui ophints

 :config linux_os
 )
