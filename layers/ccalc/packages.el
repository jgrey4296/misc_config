(defconst ccalc-packages '(
                           (ob-ccalc :location local)
                           ))


(defun ccalc/init-ob-ccalc ()
  (use-package ob-ccalc
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((ccalc . t)))
    (push '("ccalc" . prolog) org-src-lang-modes)
    )
  )
