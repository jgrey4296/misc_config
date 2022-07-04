;;; log-minor-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: June 21, 2022
;; Modified: June 21, 2022
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
;;  A Minor Mode for easily working with log files
;; - hide lines by severity
;; - hide lines by source
;; - shorten source paths
;; - highlight line numbers
;;
;;; Code:

(define-minor-mode log-minor-mode
  " A Minor Mode for easily working with log files "
  :lighter "log"
  ;;  :global t
  ;;   :keymap nil

  )

(defun log-minor-mode/turn-on ()
  (unless (minibufferp)
    (if (eq major-mode 'prog-mode)
        (log-minor-mode 1))
    )
  )

(define-globalized-minor-mode global-log-minor-mode log-minor-mode/turn-on)



(provide 'log-minor-mode)
;;; log-minor-mode.el ends here
