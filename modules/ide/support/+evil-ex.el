;;; +evil-ex.el -*- lexical-binding: t; -*-

(evil-ex-define-cmd "cc"          #'evil-goto-error)
(evil-ex-define-cmd "cfir[st]"    #'first-error)
(evil-ex-define-cmd "cr[ewind]"   #'first-error)
(evil-ex-define-cmd "cn[ext]"     #'next-error)
(evil-ex-define-cmd "cp[revious]" #'previous-error)

(evil-ex-define-cmd "com[pile]"   #'+evil:compile)
(evil-ex-define-cmd "er[rors]"    #'+default/diagnostics)

;; TODO (evil-ex-define-cmd "rx"          'doom:regex)             ; open re-builder
