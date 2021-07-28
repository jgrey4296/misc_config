;;; agendspeak-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 25, 2021
;; Modified: July 25, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/agendspeak-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(defvar-local agentspeak-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst agentspeak-font-lock-keywords
  (list)
  "Highlighting for agentspeak-mode"
  )

(define-derived-mode agentspeak-mode fundamental-mode
  "agentspeak"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map agentspeak-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list agentspeak-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'agentspeak-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'agentspeak-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table agentspeak-mode-syntax-table)
  ;;
  (setq major-mode 'agentspeak-mode)
  (setq mode-name "agentspeak")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.asl" . agentspeak-mode))


(provide 'agentspeak-mode)
;;; agendspeak-mode.el ends here
