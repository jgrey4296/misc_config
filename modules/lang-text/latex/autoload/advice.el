;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +latex-re-indent-itemize-and-enumerate-a (fn &rest args)
  (let ((LaTeX-indent-environment-list
         (append LaTeX-indent-environment-list
                 '(("itemize"   +latex-indent-item-fn)
                   ("enumerate" +latex-indent-item-fn)))))
    (apply fn args))
  )

;;;###autoload
(advice-add 'Latex-fill-region-as-para-do :around #'+latex-re-indent-itemize-and-enumerate-a)

;;;###autoload
(defun +latex-dont-indent-itemize-and-enumerate-a (fn &rest args)
  (let ((LaTeX-indent-environment-list LaTeX-indent-environment-list))
    (delq! "itemize" LaTeX-indent-environment-list 'assoc)
    (delq! "enumerate" LaTeX-indent-environment-list 'assoc)
    (apply fn args))
)

;;;###autoload
(advice-add 'Latex-fill-region-as-paragraph :around #'+latex-dont-indent-itemize-and-enumerate-a)
