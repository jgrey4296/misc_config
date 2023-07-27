;;; checkers/syntax/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +jg-flycheck-error-< (err1 err2)
  "Determine whether ERR1 is less than ERR2 by location."
  (let* ((data1 (car err1))
         (data2 (car err2))
         (l1 (flycheck-error-line data1))
         (l2 (flycheck-error-line data2))
         (c1 (or (flycheck-error-column data1) 1))
         (c2 (or (flycheck-error-column data2) 1))
         (el1 (or (flycheck-error-end-line data1) l1))
         (el2 (or (flycheck-error-end-line data2) l2))
         (cl1 (or (flycheck-error-end-column (car err1)) 1))
         (cl2 (or (flycheck-error-end-column (car err2)) 1))
         )
    (cond ((/= l1 l2)
           (< l1 l2))
          ((/= c1 c2)
           (< c1 c2))
          ((/= el1 el2)
           (< el1 el2))
          (t (< cl1 cl2))
          )
    )
  )

;;;###autoload
(defun +jg-checkers-column-format ()
  (interactive)
  (let ((fmt tabulated-list-format))
    ;; (setq tabulated-list-format (apply #'vector (mapcar #'(lambda (x) (apply 'list (car x) (max 10 (cadr x)) (cddr x))) fmt)))
    (setq tabulated-list-format
          #'[("File" 10 t)
             ("Line" 10 +jg-flycheck-error-< :right-align t) ;; flycheck-error-list-entry-<);; :right-align t)
             ("Col" 10)
             ("Level" 35 flycheck-error-list-entry-level-<)
             ("ID" 35 t)
             (#("Message (Checker)" 0 7
                (face flycheck-error-list-error-message)
                9 16
                (face flycheck-error-list-checker-name))
              10 t)]
          )
    (tabulated-list-init-header)
    (tabulated-list-print t)
    )
  )
