;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 04, 2023
;; Modified: May 04, 2023
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

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! helm
  :commands helm
  :config
  (setq helm-completing-read-handlers-alist nil)
  (load! "+helms")
  )
(use-package! helm-gtags
  :commands (helm-gtags-mode helm-gtags-create-tags helm-gtags-find-symbol
                             helm-gtags-find-tag-other-window helm-gtags-find-tag helm-gtags-find-rtag
                             helm-gtags-select helm-gtags-parse-file helm-gtags-tags-in-this-function5
                             helm-gtags-update-tags)
  )


;;; config.el ends here
