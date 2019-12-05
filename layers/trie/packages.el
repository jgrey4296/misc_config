;; trie packages.el
;; loads second

(defconst trie-packages
  '(
    ;; package from EPA
    ;; eg: some-package
    ;; (some-package :location elpa)
    ;; (some-package :location local)
    ;; (some-package :location (recipe :fetcher github :repo "some/repo"))
    ;; (some-package :excluded t)
    ;; org
    (trie-mode :location local)
    (parsec :location elpa :step pre)
    (sequence-mode :location local)
    (explore-mode :location local)
    origami
    )
  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)
;; Use: (use-package 'name :commands :config ...
(defun trie/init-trie-mode ()
  ;; Defines all sub-trie modes: trie, trie-visual, sequence etc
  (use-package trie-mode
    :commands 'trie/start-trie-ide
    :init
    (spacemacs/set-leader-keys
      "a s" 'trie/toggle-trie-ide)
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
    )

  (add-hook 'trie-mode-hook 'org-bullets-mode)
  )

(defun trie/init-parsec ()
  (use-package parsec
    :defer t))



(defun trie/init-sequence-mode ()
  (use-package sequence-mode
    :config
    (spacemacs/declare-prefix "," "Sequence Mode Prefix")
    (evil-define-key '(normal visual) sequence-mode-map
      "l" 'sequence/user-inc-column
      "h" 'sequence/user-dec-column
      "k" 'sequence/user-dec-line
      "j" 'sequence/user-inc-line
      )
    (spacemacs/set-leader-keys-for-major-mode 'sequence-mode
      "."   'spacemacs/sequence_transient-transient-state/body
      )
    (spacemacs|define-transient-state sequence_transient
      :title "Transient Editing State for Sequences"
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
      ("n" sequence/new-table ) ;; org create table, insert
      ("v" sequence/inspect-table) ;; create a left temp buffer that shows selected column's values (plus highlights active ones)
      ("b" nil ) ;; create a right temp buffer that shows selected column's values (plus highlights active ones)
      ("i" sequence/insert-rule) ;; specify LHS and RHS, insert into factbase, insert into appropriate columns
      ("r" nil ) ;; Rename the column from default
      ("t" sequence/insert-terminal) ;; Insert an Input terminal
      ("c" sequence/centre-column) ;; Centre the current column
      ("d" nil ) ;; Delete the value at point from the table
      ("D" nil ) ;; Delete the column from the table
      ("m" nil ) ;; Merge the left connections and the right connections
      ("s" nil ) ;; sort all columns alphabetically
      )
    )
  )

(defun trie/init-explore-mode ()
  (use-package explore-mode
    :config
    (spacemacs/declare-prefix "," "Explore Mode Prefix")
    (evil-define-key '(normal visual) explore-mode-map
      ;;Add motions here
      (kbd "<RET>") 'explore/expand-entry
      ;; h,l : Move column

      )
    (evil-define-key '(insert) explore-mode-map
      (kbd "<RET>") 'explore/insert-entry
      )
    (spacemacs/set-leader-keys-for-major-mode 'explore-mode
      "."   'spacemacs/explore_transient-transient-state/body
      )
    (spacemacs|define-transient-state explore_transient
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
