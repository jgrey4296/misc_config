;;; lang/lean/config.el -*- lexical-binding: t; -*-

(use-package! company-lean
  :after lean-mode
  :init
  (advice-add #'company-lean-hook :override #'ignore)
  )
