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

(load! "+vars")
(after! evil
  (load! "+bindings")
  )

(use-package! helm
  :commands helm
  :config
  (setq helm-completing-read-handlers-alist nil)
  (load! "+helms")
  )
(use-package! helm-files :defer t)
(use-package! helm-gtags :defer t)


;;; config.el ends here