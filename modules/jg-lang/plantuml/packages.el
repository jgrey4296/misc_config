;;; packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

(package! flycheck-plantuml)
(package! plantuml-mode)
(package! ob-plantuml   :recipe `(:local-repo ,(expand-file-name "packages/org-babel-ext" doom-user-dir) :files ("ob-plantuml.el")))

;;; packages.el ends here
