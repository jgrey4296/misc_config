;; -*- lexical-binding: t -*-

(defun +jg-ibuffer-update ()
  (message "jg Updating ibuffer: %s" (current-time-string))
  (ibuffer-clear-filter-groups)
  (ibuffer-filter-disable)

  ;; (ibuffer-switch-to-saved-filter-groups "my-default")
  ;; (ibuffer-switch-to-saved-filters "anti-[Helm|Magit|Help]")
  )

(defun +jg-ibuffer-reapply-filters ()
  (message "Applying Ibuffer Filters: %s" (hash-table-keys jg-ibuffer-filters))
  (message "Applying Ibuffer Groups: %s" (hash-table-keys jg-ibuffer-filter-groups))
  (setq ibuffer-saved-filters
        (cl-loop for table being the hash-values of jg-ibuffer-filters
                 append (cl-loop for key being the hash-keys of table
                                  using (hash-values val)
                                  collect (cons key (list val))
                                  )

                 )

        ibuffer-saved-filter-groups
        (cl-loop for table being the hash-values of jg-ibuffer-filter-groups
                 append (cl-loop for key being the hash-keys of table
                                  using (hash-values val)
                                  collect (cons key (list val))
                                  )

                 )
        )
  )

(cl-defun +jg-ibuffer-define-filters (sym &rest def)
  (setq jg-ibuffer-used-filter-names
        (+jg-ibuffer-define-filters-or-groups jg-ibuffer-filters jg-ibuffer-used-filter-names sym def))
  )

(cl-defun +jg-ibuffer-define-groups (sym &rest def)
  (setq jg-ibuffer-used-group-names
        (+jg-ibuffer-define-filters-or-groups jg-ibuffer-filter-groups jg-ibuffer-used-group-names sym def))
  )

(defun +jg-ibuffer-define-filters-or-groups (target names sym def)
  " Define a filter. if :override is first value after sym,
clear the saved filters for the symbol "
  (interactive)
  (when (or (and (eq (car def) :override) (pop def))
            (null (gethash sym target)))
    (let ((table (gethash sym target nil)))
      (setq names (--filter (and table (gethash it table nil)) names))
      (puthash sym (make-hash-table :test 'equal) target)
      )
    )
  (while (eq (type-of (car-safe def)) 'string)
    (if (-contains? names (car-safe def))
        (display-warning 'jg-ibuffer-filters (format "Duplicate Ibuffer Spec Defined: %s : %s" (pop def) (pop def)))
      (push (car-safe def) names)
      (puthash (pop def) (pop def) (gethash sym target)))
    )
  names
  )
