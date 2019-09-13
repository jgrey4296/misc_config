(defconst logic-packages
  '(
    ob-prolog
    (ob-ccalc :location local)
    (ob-clingo :location local)
    )
)


(defun logic/init-ob-ccalc ()
  (use-package ob-ccalc
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((ccalc . t)))
    (push '("ccalc" . prolog) org-src-lang-modes)
    )
  )
(defun logic/init-ob-clingo ()
  (use-package ob-clingo
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((clingo . t)))
    (push '("clingo" . prolog) org-src-lang-modes)
    )

  )
(defun logic/init-ob-prolog ()
  (use-package ob-prolog
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((prolog . t)))
    )
  )

