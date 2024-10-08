;;; packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: August 20, 2023
;; Modified: August 20, 2023
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

(package! centered-cursor-mode)
(package! evil-states-plus :recipe (:host github :repo "jgrey4296/evil-states-plus" :includes (move-states)))
(package! better-jumper)
(package! imenu)
(package! avy)
;;; packages.el ends here
