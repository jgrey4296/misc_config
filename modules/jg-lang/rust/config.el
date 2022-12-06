;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 19, 2022
;; Modified: May 19, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/config
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(load! "+vars")
(load! "+ivy")

(after! jg-bindings-total
  (load! "+bindings")
  )


(use-package! cargo-makefile-mode)

;;; config.el ends here
