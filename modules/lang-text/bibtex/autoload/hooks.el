;;; util/+jg-bibtex/+hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-font-lock-mod-hook ()
  (pushnew!
   bibtex-font-lock-keywords
   '(" title.+$" (0 '(:background "mediumpurple4")))
   '("\\(file\\).+?=" (1 '(:background "darkgoldenrod")))
   '("\\(tags\\).+?=.+$" (0 '(:background "darkseagreen")))
   )
  )
