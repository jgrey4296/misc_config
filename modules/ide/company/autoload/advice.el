;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +company--abort-previous-a (&rest _)
    " Allow users to switch between backends on the fly. E.g. C-x C-s followed
   by C-x C-n, will switch from `company-yasnippet' to
   `company-dabbrev-code'. "
    (company-abort)
    )

;;;###autoload
(advice-add 'company-begin-backend :before #'+company--abort-previous-a)
