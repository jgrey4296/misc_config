;;; domain-specific/window-templates/config.el -*- lexical-binding: t; -*-

;;Utilities
(defun jg-window-templates/trie-ide-running-p ()
  " Tests whether the ide is running or not "
  jg-window-templates/trie-ide-is-running
  )
(defun jg-window-templates/no-op ()
  (interactive)
  )

;;Startup and Cleanup


;;Window setup
(cl-defun jg-window-templates/build-ide-window-layout ()
  """ Setup rule editing windows """
  ;; (terminals - ) priors - rule - posts (terminals)
  ;;                       defeaters
  ;;       upstream stats  - alts - downstream stats
  (interactive)
  (let (prior post rule miscL miscC miscR)
    (delete-other-windows)
    ;; split in half
    (setq prior (selected-window))
    (setq miscL (split-window-below))
    ;;Top half:
    ;; Split into three: priors, rule, posts
    (setq rule (split-window-right))
    (select-window rule)
    (setq post (split-window-right))
    ;;Bottom Half
    ;; Split into three: upstream, alts, downstream
    (select-window miscL)
    (setq miscC (select-window (split-window-right)))
    (setq miscR (split-window-right))

    (list :prior prior :post post :rule rule :miscL miscL :miscC miscC :miscR miscR)
    )
  )
(defun jg-window-templates/show-side-window (buffer &optional left)
  (interactive)
  ;; For Terminals:
  (display-buffer-in-side-window buffer `((side . ,(if left 'left 'right))))
  )

;; Split by a grammar:
;; Num -> Columns
;; Num,Num -> Columns. Rows
