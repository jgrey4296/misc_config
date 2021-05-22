;;; domain-specific/acab-ide/+bindings.el -*- lexical-binding: t; -*-
;; Defines all sub-trie modes: trie, trie-visual, sequence etc
;; "a s t" 'jg-trie-layer/toggle-trie-ide)

(map! :leader
      :prefix "o"
      "s e" 'trie-explore/explore-current-buffer)


(map! :map trie-mode-map
      :n "#" 'trie/insert-tag
      :n "C" 'trie/insert-condition
      :n "A" 'trie/insert-action
      :n "T" 'trie/insert-transform
      )

(map! :map trie-sequence-mode-map
      :nv "l" 'trie-sequence/user-inc-column
      :nv "h" 'trie-sequence/user-dec-column
      :nv "k" 'trie-sequence/user-dec-line
      :nv "j" 'trie-sequence/user-inc-line
      :nv "."   'hydra-trie-sequence/body
      (:prefix ("," . "Sequence Prefix"))
      )

(map! :map trie-explore-mode-map
      (:prefix ("," . "Trie-Explore Mode Prefix"))
      "i n" 'trie-explore/initial-setup
      "i N" #'(lambda () (interactive) (trie-explore/initial-setup t))
      ;;Add motions here
      :nv "RET" 'trie-explore/expand-entry
      :i  "RET" 'trie-explore/insert-entry
      ;; "\t" 'jg-trie-layer/no-op
      "\t" 'trie-explore/update-tree-data
      ;; h,l : Move column
      "h" 'trie-explore/layer-decrease
      "l" 'trie-explore/layer-increase
      ;;Insertion
      "I" 'trie-explore/insert-at-leaf
      ;;Deletion
      "D" 'trie-explore/delete-entry
      :localleader
      "."   'trie-explore_transient/body
      )

(map! :map 'trie-minor-mode-map
      "f r" 'jg-trie-layer/rule-helm
      "f t" 'jg-trie-layer/type-helm
      "f T" 'jg-trie-layer/test-helm
      "f c" 'jg-trie-layer/crosscut-helm
      "f s" 'jg-trie-layer/pattern-helm
      "d r" 'jg-trie-layer/delete-rule
      "d t" 'jg-trie-layer/delete-type
      "d c" 'jg-trie-layer/delete-crosscut
      "d s" 'jg-trie-layer/delete-sequence
      "l r" 'jg-trie-layer/list-rules
      "l t" 'jg-trie-layer/list-types
      "l c" 'jg-trie-layer/list-crosscuts
      "l s" 'jg-trie-layer/list-sequences
      "?"   'trie-help-hydra/body

      "e"   'jg-trie-layer/explore-trie

      :n "[ [" 'jg-trie-layer/decrement-priors-layer
      :n "] [" 'jg-trie-layer/increment-priors-layer
      :n "[ ]" 'jg-trie-layer/decrement-posts-layer
      :n "] ]" 'jg-trie-layer/increment-posts-layer
      )
