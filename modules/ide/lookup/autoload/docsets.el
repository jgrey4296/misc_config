;;; tools/lookup/autoload/docsets.el -*- lexical-binding: t; -*-

(defvar dash-docs-docsets nil)

;;;###autoload
(defun +lookup-dash-docsets-backend-fn (identifier)
  "Looks up IDENTIFIER in available Dash docsets, if any are installed.

This backend is meant for `+lookup-documentation-functions'.

Docsets must be installed with one of the following commands:

+ `dash-docs-install-docset'
+ `dash-docs-install-docset-from-file'
+ `dash-docs-install-user-docset'
+ `dash-docs-async-install-docset'
+ `dash-docs-async-install-docset-from-file'

Docsets can be searched directly via `+lookup/in-docsets'."
  (when (require 'dash-docs nil t)
    (when-let (docsets (cl-remove-if-not #'dash-docs-docset-path (dash-docs-buffer-local-docsets)))
      (+lookup/in-docsets nil identifier docsets)
      'deferred)))

;;
;;; Commands

(defun +lookup--consult-search (sync cb)
  (lambda (action)
    (pcase action
      ((pred stringp)
       (when-let (cands (with-current-buffer cb
                          (dash-docs-search action)))
         (funcall sync 'flush)
         (funcall sync cands)))
      (_ (funcall sync action)))))

;;;###autoload
(defun +lookup/in-docsets (arg &optional query docsets)
  "Lookup QUERY in dash DOCSETS.

QUERY is a string and docsets in an array of strings, each a name of a Dash
docset. Requires either helm or ivy.

If prefix ARG is supplied, search all installed installed docsets. They can be
installed with `dash-docs-install-docset'."
  (interactive "P")
  (require 'dash-docs)
  (let ((dash-docs-common-docsets)
        (dash-docs-docsets
         (if arg
             (dash-docs-installed-docsets)
           (cl-remove-if-not #'dash-docs-docset-path (or docsets dash-docs-docsets))))
        (query (doom-thing-at-point-or-region query)))
    (doom-log "Searching docsets %s" dash-docs-docsets)
    (counsel-dash query))
)

;;;###autoload
(defun +lookup/in-all-docsets (&optional query)
  "TODO"
  (interactive)
  (+lookup/in-docsets t query))
