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
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(use-package! conf-mode
  :defer t
  :config

  (add-hook! 'conf-toml-mode-hook :depth 100
             #'outline-minor-mode
             (setq-local jg-text-whitespace-clean-hook
                         '(+jg-toml-cleanup-ensure-newline-before-table
                           delete-trailing-whitespace
                           +jg-text-cleanup-whitespace)
                         )
             )
  )
;;
;;; config.el ends here
