 ;;; tagging-minor-mode.el -*- lexical-binding: t; no-byte-compile: t;-*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 23, 2023
;; Modified: March 23, 2023
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
;;  A minor mode that allows tagging constructs in any mode it has handlers for
;;
;;; Code:

;;-- end header

(require 'evil)
(require 'tagging-minor-mode-helm-tagger)
(require 'tag-charting)
(require 'f)

(defvar tagging-minor-mode-global-tags      (make-hash-table :test 'equal))

(defvar-local tagging-minor-mode-local-tags (make-hash-table :test 'equal))

(defvar tagging-minor-mode-marker           (make-marker))

(defvar tagging-minor-mode-substitution-sources  (expand-file-name "~/github/jgrey4296.github.io/resources/tags/substitutions"))

(defvar tagging-minor-mode-main-loc         (expand-file-name "~/github/jgrey4296.github.io/.temp/tags/totals.tags"))

(defvar tagging-minor-mode-all-tags         nil)

(defvar-local tagging-minor-mode-handlers '(:new tagging-minor-mode-new-tag-default
                                            :set tagging-minor-mode-set-tags-default
                                            :get tagging-minor-mode-get-tag-default
                                            :buff tagging-minor-mode-buffer-tags-default
                                            )
                                            "A plist of handlers for tagging")

;;-- mode def

(define-minor-mode tagging-minor-mode
  "  "
  :init-value nil
  :lighter "tagging"
  ;; :global t
  ;; :keymap nil

  )

(defun tagging-minor-mode/turn-on ()
  (unless (minibufferp)
    (tagging-minor-mode 1)
    )
  )

(define-globalized-minor-mode global-tagging-minor-mode tagging-minor-mode tagging-minor-mode/turn-on)

;;-- end mode def

(defun tagging-minor-mode-random-selection  (n)
  (interactive "nHow many tags? ")
  (let* ((tags (hash-table-keys tagging-minor-mode-global-tags))
         (selection (mapcar (lambda (x) (seq-random-elt tags)) (make-list n ?a)))
         )
    (with-temp-buffer-window "*Rand Tags*"
                             'display-buffer-pop-up-frame
                             nil
                             (mapc (lambda (x) (princ x ) (princ "\n")) selection)
                             )
    )
  )

;;-- defaults

(defun tagging-minor-mode-set-tags-default (x)
  (warn "Default Set Tags")
  )

(defun tagging-minor-mode-new-tag-default (x)
  (warn "Default New Tag")
  )

(defun tagging-minor-mode-get-tag-default ()
  (warn "Default Get Tag")
  )

(defun tagging-minor-mode-buffer-tags-default ()
  (warn "Default Tag Bugger")
  )

;;-- end defaults


(defun tagging-minor-mode-set-tags (x)
  "Utility action to set tags. Works in org *and* bibtex files"
  (save-excursion
    (funcall (plist-get (buffer-local-value 'tagging-minor-mode-handlers (current-buffer)) :set) x)
    )
  )

(defun tagging-minor-mode-set-new-tag (x)
  "Utility action to add a new tag. Works for org *and* bibtex"
    (save-excursion
      (funcall (plist-get (buffer-local-value 'tagging-minor-mode-handlers (current-buffer)) :new) x)
    )
  )

(defun tagging-minor-mode-get-tags ()
  "Utility action to get tags for current entry"
     (save-excursion
       (funcall (plist-get (buffer-local-value 'tagging-minor-mode-handlers (current-buffer)) :get))
       )
   )

(defun tagging-minor-mode-get-buffer-tags ()
     (save-excursion
       (funcall (plist-get (buffer-local-value 'tagging-minor-mode-handlers (current-buffer)) :buff))
       )
   )

(defun tagging-minor-mode-parse-tag-file (path)
  " parse a file of tags and insert them into the global tag hash "
  (with-temp-buffer
    (insert-file path)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((tagline (split-string (buffer-substring (line-beginning-position) (line-end-position))
                                   ":" nil " +")))
        (unless (or (> (length tagline) 2) (string-empty-p (car tagline)))
          (puthash (car tagline) (string-to-number (cadr tagline))
                   tagging-minor-mode-global-tags)))
      (forward-line)
      )
    )
  )

(defun tagging-minor-mode-rebuild-tag-database ()
  "Rebuild the tag database from tagging-minor-mode-main-loc"
  (interactive)
  (clrhash tagging-minor-mode-global-tags)
  (cond ((not (f-exists? tagging-minor-mode-main-loc))
         (error "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY")
         )
        ((f-dir? tagging-minor-mode-main-loc)
         (let ((files (f-entries tagging-minor-mode-main-loc
                                 (-rpartial 'f-ext? "sub")
                                 t)))
           (message "Got Dir")
           (cl-loop for file in files
                    do
                    (tagging-minor-mode-parse-tag-file file))
           ))
        ((f-file? tagging-minor-mode-main-loc)
         (tagging-minor-mode-parse-tag-file tagging-minor-mode-main-loc))
        (t (message "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY"))
        )
  )

(evil-ex-define-cmd "t[ag]"  #'tagging-minor-mode-tagger)
(provide 'tagging-minor-mode)
;;; tagging-minor-mode.el ends here
