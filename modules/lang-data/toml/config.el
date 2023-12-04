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

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! conf-mode
  :commands conf-mode conf-toml-mode
  :config

  (add-hook! 'conf-toml-mode-hook :depth 100
             #'outline-minor-mode
             #'general-insert-minor-mode
             )

  (setq-hook! 'conf-toml-mode-hook
    outline-regexp "\[\[?[a-zA-Z0-9\.]+\]?\]"
    outline-heading-end-regexp "\n"
    )

  )

;; (use-package! toml-mode
;;   :commands toml-mode
;;   :config
;;   (add-hook! 'conf-toml-mode-hook :depth 100
;;              #'outline-minor-mode
;;              #'general-insert-minor-mode
;;              )
;;   )
;;
;;; config.el ends here
