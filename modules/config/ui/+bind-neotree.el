;;; +bind-neotree.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar jg-neotree-mode-map (make-sparse-keymap))

(map! :map jg-neotree-mode-map
      :after neotree
      :n "v"   (neotree-make-executor :file-fn 'neo-open-file-vertical-split)

      :n "i"  #'ignore
      :n "g"  #'neotree-refresh
      :n "q"  #'neotree-hide
      :n "Q"  (cmd! (kill-buffer (current-buffer)))
      :n "."  #'neotree-hidden-file-toggle
      :n "\\" #'neotree-change-root
      :n "r"  #'neotree-rename-node

      :n "h"  #'+neotree/collapse-or-up
      :n "l"  #'+neotree/expand-or-open
      :n "H"  #'neotree-select-up-node
      :n "L"  #'neotree-select-down-node
      :n "n"  #'neotree-select-next-sibling-node
      :n "N"  #'neotree-select-previous-sibling-node

      :n "RET" (neotree-make-executor :file-fn 'neo-open-file :dir-fn  'neo-open-dir)
      )

(map! :map jg-dired-mode-map
      :localleader
      "." #'+jg-ui-tree-dired-default-dir
      )

(after! neotree
  (setq neotree-mode-map jg-neotree-mode-map)
  )

;;; +bind-neotree.el ends here
