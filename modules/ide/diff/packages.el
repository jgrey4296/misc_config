;;; packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 28, 2023
;; Modified: May 28, 2023
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

;; (package!  :type 'local :recipe (:local-repo "" :files ("*.el" "data-struct/*.el" "modes/*.el" "util/*.el")))

(package! diff :built-in t)
(package! diff-hl )
(package! diff-mode :built-in t)
(package! ediff :built-in t)
(package! vdiff)
;; (package! diffgit)

;;; packages.el ends here
