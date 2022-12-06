;;; packages.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 19, 2022
;; Modified: May 19, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/packages
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(package! rustic)
(package! racer)
(package! rust-mode)

(package! cargo-makefile-mode :recipe `(:local-repo ,(expand-file-name "packages/cargo-makefile-mode" doom-user-dir)))

;;; packages.el ends here
