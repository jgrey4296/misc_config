;;; +spec-defs.el -*- lexical-binding: t; -*-

(defvar-local company-backends-sort-vals nil)

(defconst jg-company-backend-position-default 60)

(defconst jg-company-backend-positions
  '(:front 1
    :favour 25
    :mode 50
    :disfavour 75
    :back 90
    :last 100
    ))

(defun +jg-company-position-parse (val)
  "Convert (cons :priority 'backend) into (cons int 'backend)"
  (cond ((not (consp val))
         (cons jg-company-backend-position-default val))
        ((integerp (car val))
         val)
        ((and (keywordp (car val)) (plist-get jg-company-backend-positions (car val)))
         (cons (plist-get  jg-company-backend-positions (car val))
               (cdr val)))
        (t
         (cons jg-company-backend-position-default val))
        )
  )

(spec-handling-new! company nil :loop 'hook
                    :doc ""
                    :struct '()
                    ;; Create val sort if missing
                    (unless (and company-backends-sort-vals (hash-table-p company-backends-sort-vals))
                      (setq-local company-backends-sort-vals (make-hash-table)))
                    ;; Group backends by priority:
                    (mapc #'(lambda (x) (let ((pval (+jg-company-position-parse x)))
                                          (mapc
                                           (-partial #'(lambda (priority backend)
                                                         (cl-pushnew backend
                                                                     (gethash priority company-backends-sort-vals)))
                                                     (car pval))
                                           (ensure-list (cdr pval)))))
                          val)

                    ;; Set actual backends to each (:separate *group)
                    (setq-local company-backends
                                (cl-loop for key in (sort (hash-table-keys company-backends-sort-vals) #'<)
                                         collect
                                         (cons :separate (gethash key company-backends-sort-vals))
                                         )
                                )
                      )
