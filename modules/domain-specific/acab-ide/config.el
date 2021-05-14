;; trie config.el
;; loaded fourth

(load! "+vars")
(after! (evil hydra)
  (load! "+bindings")
  )
(after! org
  ;; TODO upgrade to org-superstar?
  (add-hook 'trie-mode-hook 'org-bullets-mode)
)

(use-package! trie-tree :defer nil)

(use-package! trie-sequence-mode
    :defer
    :commands (trie-sequence-mode)
    :config
    (map! :map trie-sequence-mode-map
          :nv "l" 'trie-sequence/user-inc-column
          :nv "h" 'trie-sequence/user-dec-column
          :nv "k" 'trie-sequence/user-dec-line
          :nv "j" 'trie-sequence/user-inc-line
          :nv "."   'hydra-trie-sequence/body
          (:prefix ("," . "Trie-Sequence Mode Prefix"))
      )
    ;; TODO Make hydra
    (defhydra trie-sequence_transient ()
      "
   | General           ^^| Change                    ^^| Motion             ^^| Remove              ^^| Sort                         ^^|
   |-------------------^^+---------------------------^^+--------------------^^+---------------------^^+------------------------------^^|
   | [_q_] Quit          | [_i_] Insert Rule           |                    ^^| [_d_] Delete Value    | [_s_] Sort Table Alpha         |
   | [_n_] New Table     |                           ^^|                    ^^| [_D_] Delete Column   |                              ^^|
   | [_v_] Table Inspect | [_r_] Rename Column         | [_c_] Centre Column  | [_m_] Merge Column    |                              ^^|
   | [_b_] Set Right Tab | [_t_] Insert Terminal       |                    ^^|                     ^^|                              ^^|
  "
      ("q" nil :exit t)
      ("n" trie-sequence/new-table ) ;; org create table, insert
      ("v" trie-sequence/inspect-table) ;; create a left temp buffer that shows selected column's values (plus highlights active ones)
      ("b" nil ) ;; create a right temp buffer that shows selected column's values (plus highlights active ones)
      ("i" trie-sequence/insert-rule) ;; specify LHS and RHS, insert into factbase, insert into appropriate columns
      ("r" trie-sequence/rename-column) ;; Rename the column from default
      ("t" trie-sequence/insert-terminal) ;; Insert an Input terminal
      ("c" trie-sequence/centre-column) ;; Centre the current column
      ("d" trie-sequence/delete-value) ;; Delete the value at point from the table
      ("D" trie-sequence/delete-column) ;; Delete the column from the table
      ("m" nil ) ;; Merge the left connections and the right connections
      ("s" trie-sequence/sort-table) ;; sort all columns alphabetically
      )
)
(use-package! trie-explore-mode
  :after (trie-tree)
  :commands (trie-explore-mode trie-explore/explore-current-buffer)
  :init
  (map! :leader
        "o s e" 'trie-explore/explore-current-buffer)
  :config
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
  (defhydra trie-explore_transient ()
    "
   | General           ^^|
   |-------------------^^+
   | [_q_] Quit          |
  "
    ("q" nil :exit t)
    )
  )
(use-package! trie-minor-mode
  :defer
  :commands (trie-minor-mode)
  :config
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
    )

(after! helm
  (load! "+helms")
  )
