;; jg_layer layers.el
;; Loads first
;;(configuration-layer/declare-layer )

(configuration-layer/declare-layers '(
                                      ;;customized spacemacs layers
                                      spacemacs-base
                                      spacemacs-completion
                                      spacemacs-layouts
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-misc
                                      spacemacs-ui
                                      spacemacs-ui-visual
                                      spacemacs-org

                                      ;;UTILITY
                                      helm
                                      auto-completion
                                      better-defaults
                                      git
                                      markdown
                                      org
                                      spell-checking
                                      syntax-checking
                                      version-control
                                      ibuffer
                                      gtags
                                      shell
                                      cscope
                                      gtags
                                      osx
                                      semantic
                                      ;;Personal
                                      jg_misc
                                      jg_exclusions
				                              jg_org
                                      tag-unify
                                      char-insertion
                                      music
                                      logic
                                      jg_twitter
                                      trie


                                      ;;LANGUAGES
                                      (csv :defer)
                                      (go :defer)
                                      (ruby :defer)
                                      (erlang :defer)
                                      haskell
                                      (javascript :defer)
                                      (csharp :defer)
                                      (fsharp :defer)
                                      (lua :defer)
                                      python
                                      emacs-lisp
                                      (racket :defer)
                                      (yaml :defer)
                                      (rust :defer)
                                      (scheme :defer)
                                      (graphviz :defer)
                                      (d :defer)
                                      (c-c++ :defer)
                                      (bibtex :defer)
                                      (latex :defer)
                                      (html :defer)
                                      (shaders :defer)
                                      (octave :defer)
                                      ;;prolog
                                      ;;smalltalk
))
