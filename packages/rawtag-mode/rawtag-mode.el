;; +rawtag-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-;
;;-- header
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: October 17, 2022
;; Modified: October 17, 2022
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

(require 'evil)

;;-- end header

;;-- vars
(defvar rawtags-tags-target "~/github/writing/resources/substitutions/tags/")
(defvar rawtags-people-target "~/github/writing/resources/substitutions/people.sub")



;;-- end vars

;;-- functions
(defun rawtags-add-substitution ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^## ")
      (let* ((current (substring (current-word) 0 -4))
             (val (ivy-read "Select Replacement: "  jg-tag-global-tags :initial-input current  :require-match t))
             )
        (end-of-line)
        (insert " : " val)
        )
      )
    (forward-line)
    )
  )

(defun rawtags-refile-tag ()
  " Refile a tag to the appropriate file
Maybe add a substitution "
  (interactive)
  (beginning-of-line)
  (unless (looking-at "^## ")
    (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (first-letter (buffer-substring-no-properties (line-beginning-position) (1+ (line-beginning-position))))
           (filename (format "%s.sub" (downcase first-letter)))
           )
      (cond ((string-empty-p line)
             (message "Empty Line"))
            ((f-exists? (f-join rawtags-tags-target filename))
             (message "First Letter: %s, File: %s" first-letter filename)
             (append-to-file (s-append "\n" line) nil (f-join rawtags-tags-target filename))
             )
            (t (message "First Letter: %s, File: %s" first-letter "symbols.sub")
               (append-to-file (s-append "\n" line)  nil (f-join rawtags-tags-target "symbols.sub"))
               )
            )
      (insert "## ")
      )
    )
  (forward-line)
  )

(defun rawtags-refile-person ()
  "refile a person to the people.sub
  maybe add a substitution"
  (interactive)
  (beginning-of-line)
  (unless (looking-at "^## ")
    (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           )
      (cond ((string-empty-p line)
             (message "Empty Line"))
            ((f-exists? rawtags-people-target)
             (message "Appending to %s" rawtags-people-target)
             (append-to-file (s-append "\n" line) nil rawtags-people-target)
             )
            (t (message "No file to add people to"))
            )
      (insert "## ")
      )
    )
  (forward-line)
  )

(defun rawtags-get-similar ()
  "Get similar tags by edit distance for comparison "
  (interactive)
  ;; use popup.el's popup-create, popup-set-list popup-draw and popup-delete
  )

(defun rawtags-collect-misspellings ()
  "group words not recognised by ispell at top of buffer"
  (interactive)



  )

(defun rawtags-collect-unsubbed ()
  "group entries that don't have a substitution at the top of the buffer"
  (interactive)

  )

;;-- end functions

;;-- keymap
(defvar-local rawtag-mode-map
  (make-sparse-keymap))

;; (map! :map rawtag-mode-map
;;       :desc "Refile Tag"       :n "r" 'rawtags-refile-tag
;;       :desc "Refile Person"    :n "p" 'rawtags-refile-person
;;       :desc "Add Substitution" :n "a" 'rawtags-add-substitution
;;       :desc "Get Similar"      :n "s" 'rawtags-get-similar
;;       )

(evil-make-intercept-map rawtag-mode-map)

;;-- end keymap

;;-- font-lock
;; List of '(regex (groupnum "face")+)
(defconst rawtag-font-lock-keywords
  (list
   `(,(rx (: line-start "## " (* any) line-end))
     (0 "font-lock-comment-face"))
   `(,(rx (: line-start (group (+ graph)) (+ blank) ":" blank (+ digit) (+ blank) ":" (+ blank) (group (+ graph)) line-end))
     (1 "hi-red-b"))
   `(,(rx (: line-start (group (+ graph))))
     (1 "diff-header"))
   )
  "Highlighting for rawtag-mode"
  )

;;-- end font-lock

;;-- mode-definition
(define-derived-mode rawtag-mode fundamental-mode
  "rawtag"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rawtag-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list rawtag-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'rawtag-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'rawtag-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "## ")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table rawtag-mode-syntax-table)
  ;;
  (setq major-mode 'rawtag-mode)
  (setq mode-name "rawtag")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.rawtags$" . rawtag-mode))
(add-to-list 'auto-mode-alist '("\.sub\\(_author\\)?$" . rawtag-mode))

;;-- end mode-definition

(provide 'rawtag-mode)
;;; +rawtag-mode.el ends here
