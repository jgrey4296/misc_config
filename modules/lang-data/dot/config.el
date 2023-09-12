;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: June 28, 2023
;; Modified: June 28, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defer-load! "+vars")
;; (defer-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")


(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :config
  (after! org
    (push '("dot" . graphviz-dot) org-src-lang-modes))
  (add-hook! 'graphviz-dot-mode-hook
             #'general-insert-minor-mode)

  )

;; https://github.com/zellerin/dynamic-graphs
(use-package! dynamic-graphs :defer t)

;;; config.el ends here
