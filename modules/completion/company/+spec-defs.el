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
                    (setq-local company-backends-sort-vals (append (mapcar #'+jg-company-position-parse val)
                                                                   company-backends-sort-vals)
                                company-backends (seq-uniq (mapcar #'cdr
                                                                   (sort company-backends-sort-vals
                                                                         #'(lambda (a b) (< (car a) (car b)))
                                                                         )))
                                )
                    )
