;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 23, 2023
;; Modified: March 23, 2023
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

(defer-load! jg-bindings-total "+bindings")
(defer-load! (jg-bindings-total dired) "+funcs")
(after! dired
  (add-hook! 'dired-mode-hook #'fd-dired-minor-mode)
  )

(after! (evil-escape evil-search)
  (add-hook 'evil-escape-hook #'evil-ex-nohighlight)
  )

;;; config.el ends here
