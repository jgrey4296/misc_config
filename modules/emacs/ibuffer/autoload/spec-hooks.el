;;; +specs.el -*- lexical-binding: t; -*-

(defvar jg-ibuffer-filter-specs (make-hash-table) "Hashtable of hashtables for defining ibuffer filters")
(defvar jg-ibuffer-group-specs  (make-hash-table) "Hashtable of hashtables for defining ibuffer filter groups")
(defvar jg-ibuffer-used-filter-names '())
(defvar jg-ibuffer-used-group-names '())

;;;###autodef
(cl-defun +jg-ibuffer-add-filter-spec (sym &rest def)
  (setq jg-ibuffer-used-filter-names
        (+jg-ibuffer-define-filters-or-groups jg-ibuffer-filter-specs
                                              jg-ibuffer-used-filter-names
                                              sym def))
  )

;;;###autodef
(cl-defun +jg-ibuffer-add-group-spec (sym &rest def)
  (setq jg-ibuffer-used-group-names
        (+jg-ibuffer-define-filters-or-groups jg-ibuffer-group-specs
                                              jg-ibuffer-used-group-names
                                              sym def))
  )

(defun +jg-ibuffer-define-filters-or-groups (target names sym def)
  " Define a filter. if :override is first value after sym,
clear the saved filters for the symbol "
  (interactive)
  ;; (message "Head: %s Names: %s" (car-safe def) names)
  (unless (gethash sym target nil)
    (puthash sym (make-hash-table :test 'equal) target))
  (when (and (eq (car def) :override) (pop def))
    (message "Got Override")
    (let ((table (gethash sym target nil)))
      (setq names (--remove (or (null table) (gethash it table nil)) names))
      (puthash sym (make-hash-table :test 'equal) target)
      )
    )
  ;; (message "Names: %s" names)
  (while (eq (type-of (car-safe def)) 'string)
    (if (-contains? names (car-safe def))
        (display-warning 'jg-ibuffer-filter-specs (format "Duplicate Ibuffer Spec Defined: %s : %s" (pop def) (pop def)))
      (push (car-safe def) names)
      (puthash (pop def) (pop def) (gethash sym target)))
    )
  names
  )

;;;###autodef
(defun +jg-ibuffer-extend-group (sym key &rest defs)
  ;; Get it or make it
  (let* ((table (or (gethash sym jg-ibuffer-group-specs nil)
                    (puthash sym (make-hash-table :test 'equal jg-ibuffer-group-specs))
                    ))
         )
    (cl-loop for entry in defs
             do
             (push entry (gethash key table))
             )
    )
  )

;;;###autodef
(defun +jg-ibuffer-reapply-specs ()
  (message "Applying Ibuffer Filters: %s" (hash-table-keys jg-ibuffer-filter-specs))
  (message "Applying Ibuffer Groups: %s" (hash-table-keys jg-ibuffer-group-specs))
  (setq ibuffer-saved-filters
        (cl-loop for table being the hash-values of jg-ibuffer-filter-specs
                 append (cl-loop for key being the hash-keys of table
                                  using (hash-values val)
                                  collect (cons key (list val))
                                  )

                 )

        ibuffer-saved-filter-groups
        (cl-loop for table being the hash-values of jg-ibuffer-group-specs
                 append (cl-loop for key being the hash-keys of table
                                  using (hash-values val)
                                  collect (cons key val)
                                  )

                 )
        )
  )
