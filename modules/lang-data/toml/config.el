;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: January 26, 2023
;; Modified: January 26, 2023
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

(load! "+vars")
(load! "+funcs")
(after! evil
  (load! "+bindings")
  )

(use-package! conf-mode
  :defer t
  :config
  (defun +jg-toml-customisation-hook ()
    (add-hook 'jg-text-whitespace-clean-hook '+jg-toml-cleanup-ensure-newline-before-table 5 t)
    (add-hook 'jg-text-whitespace-clean-hook 'delete-trailing-whitespace 10 t)
    (add-hook 'jg-text-whitespace-clean-hook '+jg-text-cleanup-whitespace 20 t)
    )

  (add-hook! 'conf-toml-mode-hook :depth 100
             #'+jg-toml-customisation-hook
             )
  )
;;
;;; config.el ends here
