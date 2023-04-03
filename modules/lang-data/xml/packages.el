;;; packages.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: September 15, 2022
;; Modified: September 15, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; (package!  :type 'local :recipe (:local-repo "" :files ("*.el" "data-struct/*.el" "modes/*.el" "util/*.el")))

(package! nxml-mode :built-in t)
(package! mhtml-mode)

;;; packages.el ends here
