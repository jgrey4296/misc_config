;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 30, 2023
;; Modified: May 30, 2023
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
;; (load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")

(use-package! calendar
  :defer t
  :config
  (add-hook! 'calendar-mode-hook #'diary-mark-entries)

  )

;;; config.el ends here
