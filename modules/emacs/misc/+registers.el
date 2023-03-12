;;; +registers.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 20, 2022
;; Modified: March 20, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/+registers
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun +jg-registers-clear-all ()
  (interactive)
  (message "Clearing")
  (setq register-alist nil)
  )


;;; +registers.el ends here
