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
