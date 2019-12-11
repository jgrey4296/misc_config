;; trie packages.el
;; loads second

(defconst trie-packages
  '(
    (trie-face :location local)
    (trie-tree :location local)
    (trie-mode :location local)
    (trie-sequence-mode :location local)
    (trie-explore-mode :location local)
    (parsec :location elpa :step pre)
    (font-lock+ :location (recipe :fetcher git :url "https://github.com/emacsmirror/font-lock-plus"))
    helm
    )
  )

;; Use: (use-package 'name :commands :config ...
(defun trie/post-init-font-lock+ ()
  (use-package font-lock+)
  )
(defun trie/init-parsec ()
  (use-package parsec
    :defer t)
  )

(defun trie/init-trie-tree ()
  (use-package trie-tree)
  )
(defun trie/init-trie-face ()
  (use-package trie-face)
  )

(defun trie/init-trie-mode ()
  ;; Defines all sub-trie modes: trie, trie-visual, sequence etc
  (use-package trie-mode
    :after (trie-face)
    :commands (trie-mode trie-log-mode trie-passive-mode)
    :init
    (spacemacs/declare-prefix "a s" "Start Editor")
    (spacemacs/set-leader-keys
      "a s t" 'trie/toggle-trie-ide)
    :config
    ;;Setup Each Mode:
    ;;Trie
    (spacemacs/set-leader-keys-for-major-mode 'trie-mode
      "f r" 'trie/find-or-create-rule
      "f t" 'trie/find-or-create-type
      "f c" 'trie/find-or-create-crosscut
      "f s" 'trie/find-or-create-sequence
      "d r" 'trie/delete-rule
      "d t" 'trie/delete-type
      "d c" 'trie/delete-crosscut
      "d s" 'trie/delete-sequence
      "l r" 'trie/list-rules
      "l t" 'trie/list-types
      "l c" 'trie/list-crosscuts
      "l s" 'trie/list-sequences
      "?"   'trie/help-hydra
      )
    (evil-define-key 'normal trie-mode-map
      (kbd "#") 'trie/insert-tag
      (kbd "C") 'trie/insert-condition
      (kbd "A") 'trie/insert-action
      (kbd "T") 'trie/insert-transform
      )

    ;;Trie passive
    (spacemacs/set-leader-keys-for-major-mode 'trie-passive-mode
      "f r" 'trie/find-or-create-rule
      "f t" 'trie/find-or-create-type
      "f c" 'trie/find-or-create-crosscut
      "f s" 'trie/find-or-create-sequence
      "f n" 'trie/find-from-snippet
      "d r" 'trie/delete-rule
      "d t" 'trie/delete-type
      "d c" 'trie/delete-crosscut
      "d s" 'trie/delete-sequence
      "l r" 'trie/list-rules
      "l t" 'trie/list-types
      "l c" 'trie/list-crosscuts
      "l s" 'trie/list-sequences
      "?"   'trie/help-hydra
      )
    (evil-define-key 'normal trie-passive-mode-map
      (kbd "<") 'trie/decrement-visual-layer
      (kbd ">") 'trie/increment-visual-layer
      (kbd "RET") 'trie/insert-into-working-rule
      )

    ;;Trie Log
    (spacemacs/set-leader-keys-for-major-mode 'trie-log-mode
      "f r" 'trie/rule-helm
      "f t" 'trie/find-or-create-type
      "f c" 'trie/find-or-create-crosscut
      "f s" 'trie/find-or-create-sequence
      "d r" 'trie/delete-rule
      "d t" 'trie/delete-type
      "d c" 'trie/delete-crosscut
      "d s" 'trie/delete-sequence
      "l r" 'trie/list-rules
      "l t" 'trie/list-types
      "l c" 'trie/list-crosscuts
      "l s" 'trie/list-sequences
      "?"   'trie/help-hydra
      )
    )

  (add-hook 'trie-mode-hook 'org-bullets-mode)
  )
(defun trie/init-trie-sequence-mode ()
  (use-package trie-sequence-mode
    :commands (trie-sequence-mode)
    :config
    (spacemacs/declare-prefix "," "Trie-Sequence Mode Prefix")
    (evil-define-key '(normal visual) trie-sequence-mode-map
      "l" 'trie-sequence/user-inc-column
      "h" 'trie-sequence/user-dec-column
      "k" 'trie-sequence/user-dec-line
      "j" 'trie-sequence/user-inc-line
      )
    (spacemacs/set-leader-keys-for-major-mode 'trie-sequence-mode
      "."   'spacemacs/trie-sequence_transient-transient-state/body
      )
    (spacemacs|define-transient-state trie-sequence_transient
      :title "Transient Editing State for Trie-Sequences"
      :doc (concat "
   | General           ^^| Change                    ^^| Motion             ^^| Remove              ^^| Sort                         ^^|
   |-------------------^^+---------------------------^^+--------------------^^+---------------------^^+------------------------------^^|
   | [_q_] Quit          | [_i_] Insert Rule           |                    ^^| [_d_] Delete Value    | [_s_] Sort Table Alpha         |
   | [_n_] New Table     |                           ^^|                    ^^| [_D_] Delete Column   |                              ^^|
   | [_v_] Table Inspect | [_r_] Rename Column         | [_c_] Centre Column  | [_m_] Merge Column    |                              ^^|
   | [_b_] Set Right Tab | [_t_] Insert Terminal       |                    ^^|                     ^^|                              ^^|
  ")
      :bindings
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
  )
(defun trie/init-trie-explore-mode ()
  (use-package trie-explore-mode
    :after (trie-tree)
    :commands (trie-explore-mode)
    :config
    (spacemacs/declare-prefix "," "Trie-Explore Mode Prefix")
    (spacemacs/set-leader-keys-for-major-mode 'trie-explore-mode
      "i n" 'trie-explore/initial-setup
      "i N" #'(lambda () (interactive) (trie-explore/initial-setup t))
      )
    (evil-define-key '(normal visual) trie-explore-mode-map
      ;;Add motions here
      (kbd "<RET>") 'trie-explore/expand-entry
      ;; "\t" 'trie/no-op
      "\t" 'trie-explore/update-tree-data
      ;; h,l : Move column
      (kbd "h") 'trie-explore/layer-decrease
      (kbd "l") 'trie-explore/layer-increase
      ;;Insertion
      (kbd "I") 'trie-explore/insert-at-leaf
      ;;Deletion
      (kbd "D") 'trie-explore/delete-entry
      )
    (evil-define-key '(insert) trie-explore-mode-map
      (kbd "<RET>") 'trie-explore/insert-entry
      )
    (spacemacs/set-leader-keys-for-major-mode 'trie-explore-mode
      "."   'spacemacs/trie-explore_transient-transient-state/body
      )
    (spacemacs|define-transient-state trie-explore_transient
      :title "Transient Editing State for Exploring Trees"
      :doc (concat "
   | General           ^^|
   |-------------------^^+
   | [_q_] Quit          |
  ")
      :bindings
      ("q" nil :exit t)
      )
    )
  )

(defun trie/pre-init-helm ()
  ;;TODO: add helms for types, crosscuts, patterns, tests, tags,
  ;;strategies, performatives, channels
  (spacemacs|use-package-add-hook helm
    :post-config
    (setq trie/rule-helm-source
          (helm-make-source "Rule Helm" 'helm-source-sync
            :action (helm-make-actions "Open Rule" 'trie/find-rule)
            :candidates 'trie/get-rule-helm-candidates
            )
          trie/rule-helm-dummy-source
          (helm-make-source "Rule Dummy Helm" 'helm-source-dummy
            :action (helm-make-actions "Create Rule" 'trie/create-rule)
            )
          )
    )

  (defun trie/rule-helm ()
    "Helm for inserting and creating rules into trie/rule authoring mode"
    (interactive)
    (helm :sources '(trie/rule-helm-source trie/rule-helm-dummy-source)
          :full-frame t
          :buffer "*Rule Helm*"
          )
  )
)
