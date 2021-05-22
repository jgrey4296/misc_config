;;; domain-specific/acab-ide/+bindings.el -*- lexical-binding: t; -*-
;; Defines all sub-trie modes: trie, trie-visual, sequence etc
;; "a s t" 'jg-trie-layer/toggle-trie-ide)

(map! :leader
      :prefix "o"
      "s e" 'trie-explore/explore-current-buffer)

(map! :after trie-mode
      :map trie-mode-map
      :n "#" #'trie/insert-tag
      :n "C" #'trie/insert-condition
      :n "A" #'trie/insert-action
      :n "T" #'trie/insert-transform
      )

(map! :after trie-sequence-mode
      :map trie-sequence-mode-map
      :nv "l" #'trie-sequence/user-inc-column
      :nv "h" #'trie-sequence/user-dec-column
      :nv "k" #'trie-sequence/user-dec-line
      :nv "j" #'trie-sequence/user-inc-line
      :nv "." #'hydra-trie-sequence/body
      (:prefix ("," . "Sequence Prefix"))
      )

(map! :after trie-explore-mode
      :map trie-explore-mode-map
      (:prefix ("," . "Trie-Explore Mode Prefix"))
      (:prefix ("i" . "Init")
      "i n"     #'trie-explore/initial-setup
      "i N"     (cmd! (trie-explore/initial-setup t)))

      ;;Add motions here
      :nv "RET" #'trie-explore/expand-entry
      :i  "RET" #'trie-explore/insert-entry
      ;; "\t"   #'jg-trie-layer/no-op
      "TAB"      #'trie-explore/update-tree-data
      ;; h,l : Move column
      "h"       #'trie-explore/layer-decrease
      "l"       #'trie-explore/layer-increase
      ;;Insertion
      "I"       #'trie-explore/insert-at-leaf
      ;;Deletion
      "D"       #'trie-explore/delete-entry

      :localleader
      "."       #'trie-explore_transient/body
      )

(map! :after trie-minor-mode
      :map trie-minor-mode-map
      "?"      #'trie-help-hydra/body
      "e"      #'jg-trie-layer/explore-trie

      (:prefix ("f" . "Find")
      "r"    #'jg-trie-layer/rule-helm
      "t"    #'jg-trie-layer/type-helm
      "T"    #'jg-trie-layer/test-helm
      "c"    #'jg-trie-layer/crosscut-helm
      "s"    #'jg-trie-layer/pattern-helm
      )
      (:prefix ("d" . "Delete")
      "r"    #'jg-trie-layer/delete-rule
      "t"    #'jg-trie-layer/delete-type
      "c"    #'jg-trie-layer/delete-crosscut
      "s"    #'jg-trie-layer/delete-sequence
      )
      (:prefix ("l" . "List")
      "r"    #'jg-trie-layer/list-rules
      "t"    #'jg-trie-layer/list-types
      "c"    #'jg-trie-layer/list-crosscuts
      "s"    #'jg-trie-layer/list-sequences
      )

      :n "[ [" #'jg-trie-layer/decrement-priors-layer
      :n "] [" #'jg-trie-layer/increment-priors-layer
      :n "[ ]" #'jg-trie-layer/decrement-posts-layer
      :n "] ]" #'jg-trie-layer/increment-posts-layer
      )
