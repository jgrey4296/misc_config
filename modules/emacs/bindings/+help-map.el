;;; +help-map.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: December 18, 2021
;; Modified: December 18, 2021
;; Version: 0.0.1
;; Keywords: bindings
;; Homepage: https://github.com/johngrey/+help-map
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun +jg-help-map-adjust ()
  (map! :map help-map
        "4" nil
        "4" #'info-other-window
        (:prefix "b"
         "t" #'+jg-which-key-show-top-level)
        (:prefix "d"
         :desc "Regexp Syntax" "r" (cmd! (info "(elisp) Syntax of Regexps"))
         )
        (:prefix ("d p" . "packages"))
        )
  )

;;; +help-map.el ends here
