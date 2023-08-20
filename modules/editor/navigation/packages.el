;;; packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 17, 2023
;; Modified: March 17, 2023
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

(package! paren-state :recipe (:host github :repo "jgrey4296/paren-state"))
(package! avy)
(package! better-jumper)
(package! imenu)

(if (modulep! +switch-window)
    (package! switch-window)
  (package! ace-window))

(when (modulep! +numbers) (package! winum))
;;; packages.el ends here
