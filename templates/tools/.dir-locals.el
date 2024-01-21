;; Directory locals file. name .dir-locals.el
;; a List of form:
;; (mode . (list (varname . value)))

(
 (auto-mode-alist . (
                     ("."        . prog-mode)
                     ("\\.el\\'" . emacs-lisp-mode)
                     ))
 (prog-mode . ((mode . abbrev)))
 )
