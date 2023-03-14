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
(defvar rawtags-base-target (expand-file-name "~/github/jgrey4296.github.io/resources/substitutions"))
(defvar rawtags-tags-sub-target "tags")

(defvar rawtags-special-tags '((acronym . "acronyms.sub")
                               (person  . "people.sub")
                               ))


;;-- end vars

;;-- functions
(defun rawtags-add-substitution ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^## ")
      (let* ((current (current-word))
             (subsel (substring current 0 (when (> (length current) 5) -4)))
             (val (ivy-read "Select Replacement: "  jg-tag-global-tags :initial-input (concat "^" (downcase current))))
             )
        (end-of-line)
        (insert " : " (s-replace-regexp " +" "_" (s-trim val)))
        )
      )
    (forward-line)
    )
  )

(defun rawtags-sub-uppercase (invert)
  "Auto-add a substitution of the tag, but uppercase"
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^## ")
      (let* ((current (current-word)))
        (end-of-line)
        (insert " : " (s-replace-regexp " +" "_"
                                        (funcall (if invert 'downcase 'upcase)
                                                 (s-trim current))))
        )
      )
    (forward-line)
    )

  )

(defun rawtags-clear-sub ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward ":" (line-end-position) t 2)
      (forward-char -1)
      (kill-line))
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
           (full-target (expand-file-name (f-join rawtags-base-target rawtags-tags-sub-target filename)))
           )
      (cond ((string-empty-p line)
             (message "Empty Line"))
            ((f-exists? full-target)
             (message "First Letter: %s, File: %s" first-letter filename)
             (append-to-file (s-append "\n" line) nil full-target)
             )
            (t (message "First Letter: %s, File: %s" first-letter "symbols.sub")
               (append-to-file (s-append "\n" line)  nil
                               (expand-file-name (f-join rawtags-base-target rawtags-tags-sub-target "symbols.sub")))
               )
            )
      (insert "## ")
      )
    )
  (forward-line)
  )

(defun rawtags-refile-type ()
  "refile a person to the people.sub
  maybe add a substitution"
  (interactive)
  (beginning-of-line)
  (unless (looking-at "^## ")
    (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (type (ivy-read "Tag Type: " rawtags-special-tags))
           (target (expand-file-name (f-join rawtags-base-target (cdr (assoc-string type rawtags-special-tags)))))
           )
      (cond ((string-empty-p line)
             (message "Empty Line"))
            ((f-exists? target)
             (message "Appending to %s" target)
             (append-to-file (s-append "\n" line) nil target)
             )
            (t (message "No file to add tag to"))
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

(defun rawtags-has-sub-p ()
  "Test if the current line has a substitution"
  (save-excursion
    (beginning-of-line)
    (search-forward ":" (line-end-position) t 2)
    )
  )

(defun rawtags-collect-unsubbed ()
  "group entries that don't have a substitution at the top of the buffer"
  (interactive)

  )

(defun rawtags-blocks-of (n)
  "Separate tags into groups of n with a line in between block "
  (interactive "nCount:\n")
  (beginning-of-buffer)
  (while (< (point) (point-max))
    (forward-line n)
    (insert "\n")
    )
  )

;;-- end functions

;;-- keymap
(defvar-local rawtag-mode-map
  (make-sparse-keymap))

(evil-define-key 'normal rawtag-mode-map
  "~" 'rawtags-sub-uppercase
  "d" 'rawtags-clear-sub

  "a" 'rawtags-add-substitution
  "b" 'rawtags-blocks-of
  "cu" 'rawtags-sub-uppercase
  "cd" (cmd! (rawtags-sub-uppercase t))
  "r" 'rawtags-refile-tag
  "R" 'rawtags-refile-type
  "s" 'rawtags-get-similar
  )

(evil-make-intercept-map rawtag-mode-map)

;;-- end keymap

;;-- font-lock
;; List of '(regex (groupnum "face")+)
(defconst rawtag-font-lock-keywords
  (list
   ;; Comment:
   `(,(rx (: line-start "## " (* any) line-end))
     (0 "font-lock-comment-face"))
   ;; Sub def:
   `(,(rx (: line-start (group (+ graph)) (+ blank) ":" blank (+ digit)
             (+ blank) (group (+ ":" (+ blank) (+ graph) (* blank))) line-end))
     (1 "hi-red-b"))
   ;; No Sub Def
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
(add-to-list 'auto-mode-alist '("\.tags$" . rawtag-mode))
(add-to-list 'auto-mode-alist '("\.sub\\(_author\\)?$" . rawtag-mode))

;;-- end mode-definition

(provide 'rawtag-mode)
;;; +rawtag-mode.el ends here
