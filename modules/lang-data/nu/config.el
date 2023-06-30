;;; config.el -*- lexical-binding: t; -*-
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


(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(define-derived-mode nushell-mode conf-colon-mode
  "Nushell"
  " Mode for editing nushell scripts "
  (outline-minor-mode)
  )

;;; config.el ends here
