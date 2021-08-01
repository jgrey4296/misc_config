;;; abl-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 28, 2021
;; Modified: July 28, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/abl-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'abl-faces)

(defvar-local abl-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst abl-font-lock-keywords
  (list
   `(,(rx line-start (* blank) (group-n 1 (or "sequential" "parallel"))
          blank (group-n 2 "behavior")
          blank (group-n 3 (+ word))
          )
     (1 'abl-face-1)
     (2 'abl-face-2)
     (3 'abl-face-3))
   `(,(rx (or "subgoal" "act" "mental_act" "wait" "precondition"
              "succeed_step" "fail_step" "specificity" "success_test"
              "ignore_failure" "persistent" "priority" "initial_tree"
              "joint" "collective" "teammembers"))
     (0 'abl-face-1))
   `(,(rx (or "with" "register"))
     (0 'abl-face-2))
   `(,(rx (or "conflict" "wme"))
     (0 'abl-face-0))

  ;; `(,(rx )
  ;;    (subexp facename override laxmatch)
  ;;    )
   )
  "Highlighting for abl-mode"
  )

(defun abl-mode-generate-syntax-table ()
  (let ((st (copy-syntax-table java-mode-syntax-table)))
    (modify-syntax-entry ?. "."     st)
    (modify-syntax-entry ?! "."     st)
    (modify-syntax-entry ?$ "_"     st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w"     st)
    (modify-syntax-entry ?/ "<12"   st)
    (modify-syntax-entry ?\n ">"    st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()"   st)
    (modify-syntax-entry ?\[ "(]"   st)
    (modify-syntax-entry ?: ".:2"   st)
    st)
)


(define-derived-mode abl-mode java-mode
  "abl"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map abl-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list abl-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'abl-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'abl-indent-line)
  ;; (set (make-local-variable 'comment-style) '(multi-box plain))
  ;; (set (make-local-variable 'comment-start) (rx (or "//" "/*")))
  ;; (set (make-local-variable 'comment-end) (rx (or "*/" line-end)))
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table (abl-mode-generate-syntax-table))
  ;;
  (setq major-mode 'abl-mode)
  (setq mode-name "abl")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.abl" . abl-mode))

(provide 'abl-mode)
;;; abl-mode.el ends here
