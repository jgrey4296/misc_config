;;; +vars.el -*- lexical-binding: t; -*-


;;-- browse providers
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Latex Packages" "https://www.ctan.org/search?phrase=%s")
            )
  )

;;-- end browse providers
