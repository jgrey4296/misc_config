;;; emacs/jg-vc/config.el -*- lexical-binding: t; -*-

(after! evil
  (load! "+bindings")
  )

(use-package smerge-mode
  :after evil
  :init
  (map! :leader
        :desc "Merge Mode"  "g m" #'evil-conflict-merge-state
        )
  )
