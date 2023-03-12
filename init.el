;;; init.el -*- lexical-binding:          t; -*-

(doom!
 :util
 mail text-manipulation char-insert tagging
 jg-misc jg-org-unit-test jg-browse

 :lang-data
 ;; csv graphql json julia ledger toml xml yaml
 :lang-dsl
 ;; acab-ide ai-and-logic faust fortran music nix qt rest sh solidity
 :lang-strongly-typed
 ;; agda coq dotnet-langs fstar haskell idris jvm-langs lean ocaml rust scala sml
 :lang-text
 ;; +org bibtex inform latex markdown plantuml rst web
 :lang-weakly-typed
 ;; erlang-vms godot lisp-langs lua python ruby

 :jg-emacs
 bindings completion dired fold
 ui projects version-control
 ;;jg-states

 :completion
 company                                   ; the ultimate code completion backend
 ivy                                       ; a search engine for love and life

 :ui
 deft                                    ; notational velocity for Emacs
 doom                                      ; what makes DOOM look the way it does
 ;;(emoji +unicode)                        ; 🙂
 hl-todo                                   ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 hydra
 indent-guides                             ; highlighted indent columns
 ligatures                                 ; ligatures and symbols to make your code pretty again
 minimap                                   ; show a map of the code on the side
 modeline                                  ; snazzy, Atom-inspired modeline, plus API
 neotree                                   ; a project drawer, like NERDTree for vim
 ophints                                   ; highlight the region an operation acts on
 (popup +defaults)                         ; tame sudden yet inevitable temporary windows
 ;;treemacs                                ; a project drawer, like neotree but cooler
 ;; unicode                                ; extended unicode support for various languages
 vc-gutter                                 ; vcs diff in the fringe
 vi-tilde-fringe                           ; fringe tildes to mark beyond EOB
 window-select                             ; visually switch windows
 workspaces                                ; tab emulation, persistence & separate workspaces

 :editor
 (evil +everywhere                        ); come to the dark side, we have cookies
 file-templates                            ; auto-snippets for empty files
 format                                    ; automated prettiness
 ;;multiple-cursors                        ; editing in many places at once
 rotate-text                               ; cycle region at point between text candidates
 snippets                                  ; my elves. They type so I don't have to
 word-wrap                                 ; soft wrapping with language-aware indent

 :emacs
 dired                                     ; making dired pretty [functional]
 electric                                  ; smarter, keyword-based electric-indent
 ibuffer                                   ; interactive buffer management
 vc                                        ; version-control and Emacs, sitting in a tree

 :term
 ;;eshell                                  ; the elisp shell that works everywhere
 shell                                     ; simple shell REPL for Emacs
 term                                      ; basic terminal emulator for Emacs
 vterm                                   ; the best terminal emulation in Emacs

 :checkers
 syntax                                    ; tasing you for every semicolon you forget
 (spell +flyspell)                         ; tasing you for misspelling mispelling
 grammar                                   ; tasing grammar mistake every you make

 :tools
 biblio
 debugger            ; FIXME stepping through code, to help you add bugs
 editorconfig        ; let someone else argue about tabs vs spaces
 ein                 ; tame Jupyter notebooks with emacs
 (eval +overlay)     ; run code, run (also, repls)
 lookup              ; navigate your code and its documentation
 lsp
 magit               ; a git porcelain for Emacs
 make                ; run make tasks from Emacs
 rgb                 ; creating color strings
 tmux                ; an API for interacting with tmux
 tree-sitter

 :os
 (:if IS-MAC macos)                        ; improve compatibility with macOS
 tty                                       ; improve the terminal Emacs experience

 :lang
 (org +gnuplot +pomodoro +journal +noter)
 emacs-lisp                                ; drown in parentheses
 (python +conda +pyright +poetry +cython)  ; beautiful is better than ugly
 ;; agda                                      ; types of types of types of types...
 ;; coq                                       ; proofs-as-programs
 ;; csharp                                    ; unity, .NET, and mono shenanigans
 ;; data                                      ; config/data formats
 ;; elixir                                    ; erlang done right
 ;; erlang                                    ; an elegant language for a more civilized age
 ;; ess                                     ; emacs speaks statistics
 ;; faust                                   ; dsp, but you get to keep your soul
 ;; fsharp                                    ; ML stands for Microsoft's Language
 ;; fstar                                     ; (dependent) types and (monadic) effects and Z3
 ;; gdscript                                  ; the language you waited for
 ;; (haskell +dante)                          ; a language that's lazier than I am
 ;; idris                                     ; a language you can depend on
 ;; json                                      ; At least it ain't XML
 ;; julia                                     ; a better, faster MATLAB
 ;; kotlin                                    ; a better, slicker Java(Script)
 ;; latex                                     ; writing papers in Emacs has never been so fun
 ;; ;;lean                                    ; for folks with too much to prove
 ;; ledger                                  ; be audit you can be
 ;; lua                                       ; one-based indices? one-based indices
 ;; markdown                                  ; writing docs for people to ignore
 ;; nix                                     ; I hereby declare "nix geht mehr!"
 ;; ocaml                                     ; an objective camel

 ;; plantuml                                  ; diagrams for confusing people more
 ;; qt                                        ; the 'cutest' gui framework ever
 ;; racket                                    ; a DSL for DSLs
 ;; rest                                    ; Emacs as a REST client
 ;; rst                                       ; ReST in peace
 ;; (ruby +rails)                           ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
 ;; (rust +lsp)                               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 ;; scala                                     ; java, but good
 ;; scheme ;; +guile)                         ; a fully conniving family of lisps
 ;; sh                                        ; she sells {ba,z,fi}sh shells on the C xor
 ;; sml
 ;; ;;solidity                                ; do you need a blockchain? No.
 ;; web                                       ; the tubes
 ;; yaml                                      ; JSON, but readable

 :email (mu4e +gmail)
 :app   (rss +org)
 :config default
 )
