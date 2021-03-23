;;; util/jg-misc/+vars.el -*- lexical-binding: t; -*-

(setq-default shell-default-shell 'shell
              shell-protect-eshell-prompt 0
              shell-enable-smart-eshell t
              hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
              hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3")
              undo-tree-visualizer-diff t
              undo-tree-auto-save-history t
              undo-tree-enable-undo-in-region t
              ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
              ;; truncating the undo history and corrupting the tree. See
              ;; https://github.com/syl20bnr/spacemacs/issues/12110
              undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000
              undo-tree-history-directory-alist `(("." . ,(concat doom-cache-dir "undo-tree-hist/"))))
