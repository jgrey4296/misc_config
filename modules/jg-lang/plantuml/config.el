;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: December 20, 2022
;; Modified: December 20, 2022
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

(after! ob-plantuml
  (advice-add #'org-babel-execute:plantuml
              :override #'jg-ob-plantuml-execute
              '((depth . -100)))
  )

;;; config.el ends here