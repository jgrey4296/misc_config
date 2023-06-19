;;; completion/ivy/autoload/hydras.el -*- lexical-binding: t; -*-

;;; Shove in an autoload directory, watch doom fall over

(defmacro simple-test ()
  (eval (format "%s" (fake-function)))
  )

;;;###autoload
(progn
  (simple-test)
  )
