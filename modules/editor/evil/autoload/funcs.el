;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun jg-evil-escape-fix (&rest args)
  " To fix interference between evil-escape and evil-ex "
  nil
  )

;;;###autoload
(advice-add 'evil-escape--insert-func :override #'jg-evil-escape-fix)

;;;###autoload
(advice-add 'evil-escape--delete-func :override #'jg-evil-escape-fix)
