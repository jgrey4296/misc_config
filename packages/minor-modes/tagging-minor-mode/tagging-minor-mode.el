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

;;-- spec registration
(defvar tagging-minor-mode-spec-table (make-hash-table))

(defun tagging-minor-mode-add-spec  (sym setfn newfn getfn)
  " Register a mode symbol with a function/symbol to
call when the evil-ex command 't[ag]' is called"
    (puthash sym `(:set setfn :new newfn :get getfn) tagging-minor-mode-spec-table)
  )

;;-- end spec registration
(defvar tagging-minor-mode-global-tags      (make-hash-table :test 'equal))
(defvar-local tagging-minor-mode-local-tags (make-hash-table :test 'equal))
(defvar tagging-minor-mode-totals           (expand-file-name "~/github/jgrey4296.github.io/resources/tags/substitutions"))
(defvar tagging-minor-mode-marker            (make-marker))

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
    (if (eq major-mode 'prog-mode)
        (tagging-minor-mode 1))
    )
  )

(define-globalized-minor-mode global-tagging-minor-mode tagging-minor-mode/turn-on)


;;-- end mode def

(defun tagging-minor-mode-occurrences ()
  " Create a Bar Chart of Tags in the current buffer "
  (interactive)
  (let* ((depth-arg evil-ex-argument)
         (depth (if depth-arg (string-to-number depth-arg) nil))
         (alltags (make-hash-table :test 'equal))
         )
    (if (eq 'org-mode major-mode)
        (progn
          ;; (message "Getting Tags for all buffers to depth: %s" depth)
          (maphash (lambda (k v) (cl-incf (gethash k alltags 0) v)) (+jg-tag-get-buffer-tags nil depth))
          (if (not (hash-table-empty-p alltags))
              (+jg-tag-chart-tag-counts alltags (buffer-name))
            (message "No Tags in buffer")))
      (message "Not in an org buffer")
      )
    )
  )
(defun tagging-minor-mode-occurrences-in-open-buffers()
  " Retrieve all tags in all open buffers, print to a temporary buffer "
  (interactive "p")
  (let* ((allbuffers (buffer-list))
         (alltags (make-hash-table :test 'equal))
         (depth (if depth-arg (string-to-number depth-arg) nil))
         )
    ;; (message "Getting Tags for all buffers to depth: %s" depth)
    (cl-loop for x in allbuffers do
          (if (with-current-buffer x (eq 'org-mode major-mode))
              (maphash (lambda (k v) (if (not (gethash k alltags)) (puthash k 0 alltags))
                         (cl-incf (gethash k alltags) v)) (+jg-tag-get-buffer-tags x depth))
            )
          )
    (if (not (hash-table-empty-p alltags))
        (+jg-tag-chart-tag-counts alltags "Active Files")
      (message "No Tags in buffers"))
    )
  )
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

(defun +jg-tag-set-tags (x)
  "Utility action to set tags. Works in org *and* bibtex files"
  (-when-let (handlers (gethash major-mode tagging-minor-mode-spec-table nil))
      (funcall (plist-get handlers :set) x)
  )
)
(defun +jg-tag-set-new-tag (x)
  "Utility action to add a new tag. Works for org *and* bibtex"
  (-when-let (handlers (gethash major-mode tagging-minor-mode-spec-table nil))
    (funcall (plist-get handlers :new) x)
    )
  )
(defun +jg-tag-get-tags ()
  "Utility action to get tags for current entry"
   (-when-let (handlers (gethash major-mode tagging-minor-mode-spec-table nil))
     (save-excursion
       (funcall (plist-get handlers :get))
       )
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
                   jg-tag-global-tags)))
      (forward-line)
      )
    )
  )
(defun tagging-minor-mode-rebuild-tag-database ()
  "Rebuild the tag database from jg-tag-loc-global-tags"
  (interactive)
  (clrhash jg-tag-global-tags)
  (cond ((not (f-exists? jg-tag-loc-global-tags))
         (error "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY")
         )
        ((f-dir? jg-tag-loc-global-tags)
         (let ((files (f-entries jg-tag-loc-global-tags
                                 (-rpartial 'f-ext? "sub")
                                 t)))
           (message "Got Dir")
           (cl-loop for file in files
                    do
                    (tagging-minor-mode-parse-tag-file file))
           ))
        ((f-file? jg-tag-loc-global-tags)
         (tagging-minor-mode-parse-tag-file jg-tag-loc-global-tags))
        (t (message "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY"))
        )
  )

(provide 'tagging-minor-mode)
;;; tagging-minor-mode.el ends here
