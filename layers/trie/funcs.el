;; trie funcs.el
;; loaded third.

;; (when (configuration-layer/package-usedp 'package)
;;   (defun spacemacs/<package>-enable () )
;;   (defun spacemacs/<package>-disable () ))

(defun trie/setup-trie-mode-windows ()
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

    (window--display-buffer (generate-new-buffer "prior")  prior 'window)
    (window--display-buffer (generate-new-buffer "post")  post 'window)
    (window--display-buffer (generate-new-buffer "rule")  rule 'window)
    (window--display-buffer (generate-new-buffer "miscL")  miscL 'window)
    (window--display-buffer (generate-new-buffer "miscR")  miscR 'window)
    (window--display-buffer (generate-new-buffer "miscC")  miscC 'window)

    (list prior post rule miscL miscC miscR)
    )
  )

(defun trie/show-side-window (buffer &optional left)
  (interactive)
  ;; For Terminals:
  (display-buffer-in-side-window buffer `((side . ,(if left 'left 'right))))
  )
