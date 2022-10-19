;;; domain-specific/acab-ide/+bindings.el -*- lexical-binding: t; -*-
;; Defines all sub-trie modes: trie, trie-visual, sequence etc
;; "a s t" 'acab-ide/toggle-trie-ide)

(map! :leader
      :prefix "o"
      "s e" 'trie-explore/explore-current-buffer)

(map! :map acab-rule-mode-map
      :localleader
      :desc "Comint Init" "i" #'acab-comint/init
      :desc "Send Region/Line"     "e" #'acab-comint/send-region
      :desc "Report Ctxs"          "c" nil
      :desc "Save State to Buffer" "p" nil
      :desc "Acab Engine Report"   "r" nil
      :desc "Kill Comint"          "q" nil
      )

(map! :map acab-rule-mode-map
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
      ;;Add motions here
      :nv "RET" #'trie-explore/expand-entry
      :i  "RET" #'trie-explore/insert-entry
      ;; "\t"   #'acab-ide/no-op
      "TAB"      #'trie-explore/update-tree-data
      ;; h,l : Move column
      "h"       #'trie-explore/layer-decrease
      "l"       #'trie-explore/layer-increase
      ;;Insertion
      "I"       #'trie-explore/insert-at-leaf
      ;;Deletion
      "D"       #'trie-explore/delete-entry

      :localleader
      (:prefix ("i" . "Init")
       :desc "Generate Tree" "n"     #'trie-explore/initial-setup
       :desc "Generate Empty Tree" "N"     (cmd! (trie-explore/initial-setup t)))
      :desc "Transient Body" "."       #'trie-explore_transient/body
      )

(map! :after trie-minor-mode
      :map trie-minor-mode-map
      "?"      #'trie-help-hydra/body
      "e"      #'acab-ide/explore-trie

      (:prefix ("f" . "Find")
      "r"    #'acab-ide/rule-helm
      "t"    #'acab-ide/type-helm
      "T"    #'acab-ide/test-helm
      "c"    #'acab-ide/crosscut-helm
      "s"    #'acab-ide/pattern-helm
      )
      (:prefix ("d" . "Delete")
      "r"    #'acab-ide/delete-rule
      "t"    #'acab-ide/delete-type
      "c"    #'acab-ide/delete-crosscut
      "s"    #'acab-ide/delete-sequence
      )
      (:prefix ("l" . "List")
      "r"    #'acab-ide/list-rules
      "t"    #'acab-ide/list-types
      "c"    #'acab-ide/list-crosscuts
      "s"    #'acab-ide/list-sequences
      )

      :n "[ [" #'acab-ide/decrement-priors-layer
      :n "] [" #'acab-ide/increment-priors-layer
      :n "[ ]" #'acab-ide/decrement-posts-layer
      :n "] ]" #'acab-ide/increment-posts-layer
      )

;; (map! :map acab-rule-mode-map
;;       :desc "Acab-Block" :v "i f"  nil ;;#'acab-ide-block-textobj
;; :desc "Forward Section"  :vn "] ]" nil ;; #'acab-ide-forward-block-motion
;; :desc "Backward Section" :vn "[ [" nil ;;#'acab-ide-backward-block-motion
;;       )
