;; -*- lexical-binding: t -*-

(defvar jg-ibuffer-generate-group-hook nil)

(defun +jg-ibuffer-generate-project-groups ()
  "Create a set of ibuffer filter groups based on the projectile root dirs of buffers."
  (let ((roots (ibuffer-remove-duplicates
                (delq nil (mapcar 'ibuffer-projectile-root (buffer-list))))))
    (mapcar (lambda (root)
              (cons (funcall ibuffer-projectile-group-name-function (car root) (cdr root))
                    `((projectile-root . ,root))))
            roots))
  )

(defun +jg-ibuffer-run-generate-hook ()
  (append (cl-loop for fn in jg-ibuffer-generate-group-hook
                   append
                   (funcall fn)
                   )
          (when jg-ibuffer-default-group
            (cdr (assoc jg-ibuffer-default-group ibuffer-saved-filter-groups)))
          )
  )

;;;###autoload
(defun +jg-ibuffer-default()
  (interactive)
  (if (get-buffer "*Ibuffer*")
      (switch-to-buffer "*Ibuffer*")
    (ibuffer nil nil
             (when jg-ibuffer-default-filter ;; filter
               `((saved . ,jg-ibuffer-default-filter)))
             nil nil
             ;; groups
             (+jg-ibuffer-run-generate-hook)
             )
    (push jg-ibuffer-never-show-regexps ibuffer-tmp-hide-regexps)
    (+jg-ibuffer-sort-groups)
    )
  )

;;;###autoload
(defun +jg-ibuffer-add-group (name)
  (interactive (list (completing-read "Group: " ibuffer-saved-filter-groups nil t)))
  (setq ibuffer-filter-groups
        (append (cdr (assoc name ibuffer-saved-filter-groups))
                ibuffer-filter-groups))
  (ibuffer-update 0)
  )

;;;###autoload
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

;;;###autoload (autoload 'ibuffer-make-column-size "ui/ibuffer/autoload/ibuffer" nil t)
(define-ibuffer-column size
  (:name "Size")
  (file-size-human-readable (buffer-size))
  )

;;;###autoload
(defmacro ibuffer-generate! (&rest body)
  `(lambda ()
     (interactive)
     (setq ibuffer-filter-groups ,@body)
     (ibuffer-update t)
     )
  )

;;;###autoload
(defun +jg-ibuffer-print-help ()
  (interactive)
  (message "Ibuffer: (Sort: ,) (Format: .) (Filter: \) (Jump: s) (Mark: -) (Mark-all: =) (Help: ?)")
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
