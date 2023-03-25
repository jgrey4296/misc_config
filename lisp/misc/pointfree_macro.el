;;; pointfree_macro.el -*- lexical-binding: t; -*-

 ;; (funcall (-compose (-partial 'mapcar
 ;;                              (-partial 'apply 'max))
 ;;                    (-partial 'mapcar
 ;;                              (-partial 'mapcar 'length)))
 ;;          rows-of-cols)


(funcall (pointfree | 'max <- 'mapcar | 'max <- 'mapcar < 'mapcar 'length)
         '(("a" "ab") ("aweg" "AWefaw") ("a" "ba" "aca")))

(defmacro pointfree (&rest args)
  " An experiment macro for writing in a pointfree style.
ie: convert functions into partials,
apply them in the correct order

        a b c   = (-partial a b c)
        | b     = (-partial apply b)
        \ b     = (-partial funcall b)
        a < b   = (-partial a b)
        a <-  b = (-compose a b)

(funcall (pointfree | 'max <- 'mapcar | 'max <- 'mapcar < 'mapcar 'length)
         '((\"a\" \"ab\") (\"aweg\" \"AWefaw\") (\"a\" \"ba\" \"aca\"))) => 6

(funcall (pointfree \ '* 2 <- '-  : 3 <- '* 4) 5 4) => 154
"
  (let* ((base (reverse args))
         results current val other)
    (while (setq other nil val (pop base))
      (when (and (-contains? '(< <- -> >) val)
                 (> (length current) 1))
        (push '-partial current)
        )
      (cond ((and (eq val '<-)
                  (> (length current) 1))
             (push current results)
             (setq current nil)
             )
            ((and (eq val '<-)
                  (listp (car current)))
             (push (car current) results)
             (setq current nil)
             )
            ((eq val '|)
             (push ''apply current)
             (push '-partial current)
             (let ((curr current))
               (setq current nil)
               (push curr current))
             )
            ((eq val '\ )
             (push ''funcall current)
             (push '-partial current)
             (let ((curr current))
               (setq current nil)
               (push curr current))
             )
            ((eq val ':)
             (let ((curr current)
                   (other (pop base))
                   )
               (setq current nil)
               (when (eq (car-safe other) 'quote)
                 (setq other (cadr other)))
               (when (eq 1 (length curr))
                 (setq curr (car curr)))
               (push `(lambda (x) (,other x ,curr)) current)
               )
             )
            ((eq val '<)
             (let ((curr current))
               (setq current nil)
               (push curr current))
             (push (pop base) current)
             )
            (t (push val current))
            )
      )
    (cond ((> (safe-length current) 1)
           (push '-partial current)
           (push current results))
          (current
           (push (car current) results))
          )
    (if (> (length results) 1)
        (push '-compose results)
      (setq results (car results))
      )
    results
    )
  )
