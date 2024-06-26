;;; packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 21, 2023
;; Modified: April 21, 2023
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
(package! nushell-mode)
(package! nushell-ts-mode)

;;; packages.el ends here
