;;; init.el -*- lexical-binding: t; -*-
;; last working doom commit: ba1dca322f9a07bc2b7bec6a98f2c3c55c0bbd77

(setq force-load-messages nil)
(defvar templates-loc (expand-file-name "~/.config/.templates"))

(load! "defer-macro")
(doom!
 :config     default ;; +smartparens
 :editor     (evil +everywhere)
 :config     bindings disabled help search
 :config     ui ;; +light

 ;; :experimentation
 :ui (ivy) ;; +prescient +fuzzy
 :ui helm hydra ibuffer minibuffer
 :ui popup ;; +poppy

 :editor format
 :editor text-manipulation ;; +onsave
 :editor buffer-nav
 :editor window-nav ;; +numbers +ace
 :editor (fold +hideshow +vimish +shy +outline +treesit)
 :editor undo
 :editor large-files
 :editor autosave

 :ide version-control ;; +forge +diff-hl
 :ide (librarian +tags)
 :ide snippets workspaces
 :ide (support +lsp +flycheck +treesit) ;; +lsp +eglot  +semantic +tree-sitter
 :ide minimap diff company
 :ide (debugger +realgud +edebug)

 :lang-data csv dot sql nu octave
 :lang-data graphql
 :lang-data opa
 :lang-data xml toml json yaml logs
 :lang-data quotes_and_time

 ;; :lang-dsl acab
 :lang-dsl ai-and-logic
 :lang-dsl rest nix
 :lang-dsl qt
 :lang-dsl (music +sclang +csound)
 :lang-dsl make

 :lang-strongly-typed haskell
 :lang-strongly-typed (proofs) ;; coq, idris, agda, fstar, lean)
 :lang-strongly-typed (rust +llvm)
 :lang-strongly-typed jvm-langs dotnet-langs ml-langs

 :lang-text inform web
 :lang-text org ;; +unittest
 :lang-text bibtex latex markdown plantuml rst

 :lang-weakly-typed lisp-langs
 :lang-weakly-typed lua ;; +moonscript
 :lang-weakly-typed (python +isort +builtin) ;; +tox +external
 :lang-weakly-typed erlang-vms godot
 :lang-weakly-typed ruby ;; +rbenv +rvm +chruby

 :tools pdfs ledger
 :tools mail term eval
 :tools processes
 :tools calendar calc
 :tools dired ;; +dirvish

 :ui doom-ui
 :ui ophints

 :config linux_os
 )
