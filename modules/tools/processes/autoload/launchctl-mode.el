;;; launchctl-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: June 09, 2023
;; Modified: June 09, 2023
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

;;-- end header

;;-- imports
(require 'tabulated-list)
;;-- end imports

(defvar launchctl-format
  #'[
     ("PID" 10 t)
     ("Status" 10 t)
     ("Label" 10 t)
     ]
  )

(defun launchctl-entries ()
  (with-temp-buffer
    (call-process "launchctl" nil t nil "list")
    (cdr (mapcar #'(lambda (line)
                     (list nil (apply 'vector (split-string line nil t " +"))))
                 (split-string (buffer-string) "\n" t " +")))
    )
  )

(define-derived-mode launchctl-mode tabulated-list-mode
  "Launchctl"
  "For viewing daemons"
  (setq-local tabulated-list-format launchctl-format
              tabulated-list-entries #'launchctl-entries
              tabulated-list-sort-key (cons "PID" nil)
              )
  (tabulated-list-init-header)
  (tabulated-list-print)
  )

;;;###autoload
(defun +jg-processes-launchctl ()
  (interactive)
  (let ((buff (get-buffer-create "*Launchctl*")))
    (with-current-buffer buff
      (launchctl-mode)
      )
    (display-buffer buff)
    )
  )

(provide 'launchctl-mode)
;;; launchctl-mode.el ends here
