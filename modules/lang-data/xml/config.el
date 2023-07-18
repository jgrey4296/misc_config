;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: September 15, 2022
;; Modified: September 15, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(load! "+vars")

(defer-load! (jg-bindings-total jg-dired nxml-mode) "+bindings")

(use-package! mhtml-mode
  :commands mhtml-mode
  :config
  (add-hook! (mhtml-mode-hook html-mode-hook)
             #'tree-sitter!
             )
  )

(use-package! nxml-mode
  :commands nxml-mode
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  (setq-hook! 'nxml-mode-hook tab-width nxml-child-indent)
  (add-hook! 'nxml-mode-hook 'hs-minor-mode)
  )

;;; config.el ends here
