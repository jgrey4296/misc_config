;;; emacs/jg-vc/config.el -*- lexical-binding: t; -*-

(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package smerge-mode
  :after jg-bindings-total
  :init
  (map! :leader
        :desc "Merge Mode"  "g m" #'evil-conflict-merge-state
        )
  )
