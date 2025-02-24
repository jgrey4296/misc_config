;;; flycheck.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +jg-flycheck-error-< (err1 err2)
  "Determine whether ERR1 is less than ERR2 by location."
  (let* ((data1 (car err1))
         (data2 (car err2))
         (l1 (flycheck-error-line data1))
         (l2 (flycheck-error-line data2))
         (c1 (or (flycheck-error-column data1) 1))
         (c2 (or (flycheck-error-column data2) 1))
         (el1 (or (flycheck-error-end-line data1) l1))
         (el2 (or (flycheck-error-end-line data2) l2))
         (cl1 (or (flycheck-error-end-column (car err1)) 1))
         (cl2 (or (flycheck-error-end-column (car err2)) 1))
         )
    (cond ((/= l1 l2)
           (< l1 l2))
          ((/= c1 c2)
           (< c1 c2))
          ((/= el1 el2)
           (< el1 el2))
          (t (< cl1 cl2))
          )
    )
  )

;;;###autoload
(defun +jg-flycheck-column-format ()
  (interactive)
  (let ((fmt tabulated-list-format))
    ;; (setq tabulated-list-format (apply #'vector (mapcar #'(lambda (x) (apply 'list (car x) (max 10 (cadr x)) (cddr x))) fmt)))
    (setq tabulated-list-format
          #'[("File" 10 t)
             ("Line" 10 +jg-flycheck-error-< :right-align t) ;; flycheck-error-list-entry-<);; :right-align t)
             ("Col" 10)
             ("Level" 35 flycheck-error-list-entry-level-<)
             ("ID" 35 t)
             (#("Message (Checker)" 0 7
                (face flycheck-error-list-error-message)
                9 16
                (face flycheck-error-list-checker-name))
              10 t)]
          )
    (tabulated-list-init-header)
    (tabulated-list-print t)
    )
  )

(defvar jg-flycheck-id-filter nil)

;;;###autoload
(defun jg-flycheck-error-list-set-filter ()
  "Restrict the error list to of a set ID"
  (interactive)
  (let* ((ids (cl-remove-duplicates (mapcar #'flycheck-error-id (flycheck-error-list-current-errors)) :test 'equal))
         (id (ivy-read "Error to select: " ids))
         )
    (flycheck-error-list-with-buffer
      (setq jg-flycheck-id-filter (if (string-empty-p id) nil id))
      (force-mode-line-update)
      (flycheck-error-list-refresh)
      (flycheck-error-list-recenter-at (point-min)))
    )
  )

;;;###autoload
(defun jg-flycheck-error-list-apply-filter-a (errors)
  "Filter ERRORS according to set ID"
  (if-let* ((id jg-flycheck-id-filter)
            (filtered (seq-filter #'(lambda (x) (equal (flycheck-error-id x) id)) errors))
            )
      filtered
    errors
    )
  )

;;;###autoload
(defun jg-flycheck-set-filter-to-highest-actual-a ()
  "Advice to set the flycheck error list to the highest level that actually has entries.
ie: if there errors, set it to error, else warnings, else info
"
  (let* ((errors (flycheck-error-list-current-errors))
         (counts (flycheck-count-errors errors))
         (lvl (cond ((alist-get 'error counts)
                     'error)
                    ((alist-get 'warning counts)
                     'warning)
                    (t 'info)))
         )
    (flycheck-error-list-set-filter lvl)
    )
  )

(defvar jg-flycheck-filter-hook nil)
(defun +jg-flycheck-filter-by-level (err)
  (if-let* ((min-level flycheck-error-list-minimum-level)
            (min-severity (flycheck-error-level-severity min-level)))
      (>= (flycheck-error-level-severity (flycheck-error-level err)) min-severity)
    t)
  )
(defun +jg-flycheck-error-list-apply-filters (errors)
  "Alternative `flycheck-error-list-apply-filter',
to filter by registered filters in `jg-flycheck-filter-hook'.
(hook form: lambda (err) -> bool)

use as override advice on `flycheck-error-list-apply-filter'

returns a list of matching errors
"
  (add-hook 'jg-flycheck-filter-hook #'+jg-flycheck-filter-by-level)
  (seq-filter #'(lambda (err)
                  (cl-loop for pred in jg-flycheck-filter-hook
                           if (not (funcall pred err)) return nil
                           finally return t
                           ))
              errors)
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 20, 2025
;; Modified:   January 20, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; flycheck.el ends here
