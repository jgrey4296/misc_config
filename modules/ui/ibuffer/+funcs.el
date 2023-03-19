;; -*- lexical-binding: t -*-

(defun +jg-ibuffer-default()
  (interactive)
  (if (get-buffer "*Ibuffer*")
      (switch-to-buffer "*Ibuffer*")
    (ibuffer nil nil '((saved . "-clutter")) nil nil
             (cdr (assoc "default" ibuffer-saved-filter-groups)))
    (push jg-ibuffer-never-show-regexps ibuffer-tmp-hide-regexps)
    (+jg-ibuffer-sort-groups)
    )
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
                                  collect (cons key val)
                                  )

                 )
        )
  )

(defun +jg-ibuffer-add-group (name)
  (interactive (list (completing-read "Group: " ibuffer-saved-filter-groups nil t)))
  (setq ibuffer-filter-groups
        (append (cdr (assoc name ibuffer-saved-filter-groups))
                ibuffer-filter-groups))
  (ibuffer-update 0)
  )

(defun +jg-ibuffer-sort-groups ()
  (interactive)
  (setq ibuffer-filter-groups (sort ibuffer-filter-groups
                                    #'(lambda (a b)
                                        (cond ((s-starts-with? "*" (car a))
                                               nil
                                               )
                                              ((s-starts-with? "*" (car b))
                                               t
                                               )
                                              (t
                                               (string-lessp (car a) (car b))
                                               )
                                              )
                                        )
                                    ))
  (ibuffer-update 0)
  )

(cl-defun +jg-ibuffer-define-filters (sym &rest def)
  (setq jg-ibuffer-used-filter-names
        (+jg-ibuffer-define-filters-or-groups jg-ibuffer-filters
                                              jg-ibuffer-used-filter-names
                                              sym def))
  )

(cl-defun +jg-ibuffer-define-groups (sym &rest def)
  (setq jg-ibuffer-used-group-names
        (+jg-ibuffer-define-filters-or-groups jg-ibuffer-filter-groups
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
        (display-warning 'jg-ibuffer-filters (format "Duplicate Ibuffer Spec Defined: %s : %s" (pop def) (pop def)))
      (push (car-safe def) names)
      (puthash (pop def) (pop def) (gethash sym target)))
    )
  names
  )

(defun +jg-ibuffer-extend-group (sym key &rest defs)
  ;; Get it or make it
  (let* ((table (or (gethash sym jg-ibuffer-filter-groups nil)
                    (puthash sym (make-hash-table :test 'equal jg-ibuffer-filter-groups))
                    ))
         )
    (cl-loop for entry in defs
             do
             (push entry (gethash key table))
             )
    )
  )

;;-- test
;; (define-ibuffer-filter jg-projectile-root
;;     "Toggle current view to buffers with projectile root dir QUALIFIER."
;;   (:description "jg projectile root dir"
;;    :reader (read-regexp "Filter by projectile root dir (regexp): "))
;;   (+jg-test-project buf qualifier)
;;   )

;; (defun +jg-test-project (buf qual)
;;   (message "Buf: %s Qual: %s project: %s" buf qual (ibuffer-projectile-root buf))
;;   (ibuffer-awhen (ibuffer-projectile-root buf)
;;     (if (stringp qual)
;;         (or (string-match-p qual (car it))
;;             (string-match-p qual (cdr-safe it)))
;;       (equal qual it))))

;;-- end test
