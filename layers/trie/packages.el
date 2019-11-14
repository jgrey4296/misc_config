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
  (use-package trie-mode)
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

(defun trie/post-init-origami ()
  ;; (require 'trie/origami-parser "~/.spacemacs.d/layers/trie/local/origami-parser.el")
  ;; (add-to-list 'origami-parse-alist '(trie-mode . trie/origami-parser))

  )
