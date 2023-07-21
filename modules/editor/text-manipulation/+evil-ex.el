;;; +evil-ex.el -*- lexical-binding: t; -*-

(evil-ex-define-cmd "al[ign]"               #'+jg-text-manipulation-ex-align-highlight)
(evil-ex-define-cmd "ral[ign]"              #'+evil:align-right)

(evil-ex-define-cmd "hl"                    #'evil-ex-match)

(evil-ex-define-cmd "rev[erse]"             #'+evil:reverse-lines)
(evil-ex-define-cmd "l[ine]diff"            #'evil-quick-diff)

(evil-ex-define-cmd "c[opy]"                #'evil-copy)
(evil-ex-define-cmd "m[ove]"                #'evil-move)
(evil-ex-define-cmd "d[elete]"              #'evil-ex-delete)
(evil-ex-define-cmd "y[ank]"                #'evil-ex-yank)

(evil-ex-define-cmd "j[oin]"                #'evil-ex-join)

(evil-ex-define-cmd "le[ft]"                #'evil-align-left)
(evil-ex-define-cmd "ri[ght]"               #'evil-align-right)
(evil-ex-define-cmd "ce[nter]"              #'evil-align-center)
(evil-ex-define-cmd "<"                     #'evil-shift-left)
(evil-ex-define-cmd ">"                     #'evil-shift-right)

(evil-ex-define-cmd "s[ubstitute]"          #'evil-ex-substitute)
(evil-ex-define-cmd "&"                     #'evil-ex-repeat-substitute)
(evil-ex-define-cmd "&&"                    #'evil-ex-repeat-substitute-with-flags)
(evil-ex-define-cmd "~"                     #'evil-ex-repeat-substitute-with-search)
(evil-ex-define-cmd "~&"                    #'evil-ex-repeat-substitute-with-search-and-flags)

(evil-ex-define-cmd "retab"                 #'+evil:retab)

(evil-ex-define-cmd "show-digraphs"         #'evil-ex-show-digraphs)
(evil-ex-define-cmd "sor[t]"                #'evil-ex-sort)
