;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)
