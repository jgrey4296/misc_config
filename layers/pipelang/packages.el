(defconst pipelang-packages
  '(
    (ob-pipelang :location local)
    (pipelang-mode :location local)
    )
  )

(defun pipelang/init-ob-pipelang ()
  (use-package ob-pipelang
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages '((pipe . t)))
    (push '("pipe" . pipelang) org-src-lang-modes)
    )
  )

(defun pipelang/init-pipelang-mode ()
  (use-package pipelang-mode
    :defer t

    )
  )
