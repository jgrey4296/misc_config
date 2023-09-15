;;; +evil-ex.el -*- lexical-binding: t; -*-

(evil-ex-define-cmd "hl"                       #'+jg-text-manipulation-ex-match-highlight)
(evil-ex-define-cmd "hlc"                      #'evil-ex-match)

(evil-ex-define-cmd "rev[erse]"                #'+evil:reverse-lines)
(evil-ex-define-cmd "l[ine]diff"               #'evil-quick-diff)
(evil-ex-define-cmd "ld"                       #'evil-quick-diff)

(evil-ex-define-cmd "c[opy]"                   #'evil-copy)
(evil-ex-define-cmd "m[ove]"                   #'evil-move)
(evil-ex-define-cmd "d[elete]"                 #'evil-ex-delete)
(evil-ex-define-cmd "y[ank]"                   #'evil-ex-yank)

(evil-ex-define-cmd "j[oin]"                   #'evil-ex-join)

(evil-ex-define-cmd "a[lign]"                  #'+jg-text-manipulation-ex-align-highlight)
(evil-ex-define-cmd "A"                        #'+jg-text-manipulation-ex-expand-align-highlight)
(evil-ex-define-cmd ":"                        #'+jg-text-manipulation-ex-expand-align-highlight)
(evil-ex-define-cmd "la"                       #'evil-align-left)
(evil-ex-define-cmd "ra"                       #'evil-align-right)
(evil-ex-define-cmd "ce[nter]"                 #'evil-align-center)
(evil-ex-define-cmd "<"                        #'evil-shift-left)
(evil-ex-define-cmd ">"                        #'evil-shift-right)

(evil-ex-define-cmd "s[ubstitute]"             #'evil-ex-substitute)
(evil-ex-define-cmd "S[ubstitute]"             #'evil-ex-substitute)
(evil-ex-define-cmd "sr"                       #'evil-ex-repeat-substitute)
(evil-ex-define-cmd "srf"                      #'evil-ex-repeat-substitute-with-flags)

(evil-ex-define-cmd "show-digraphs"            #'evil-ex-show-digraphs)
(evil-ex-define-cmd "sor[t]"                   #'evil-ex-sort)

(evil-ex-define-cmd "sel"                      #'+jg-text-manipulation-ex-expand-selection)

;; TODO add a move-line/region-to-marker
