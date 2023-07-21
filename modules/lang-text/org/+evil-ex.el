;;; +evil-ex.el -*- lexical-binding: t; -*-


(evil-ex-define-cmd "cap[ture]" #'org-capture)
(evil-ex-define-cmd "os"        #'org-store-link)
(evil-ex-define-cmd "oi"        #'org-insert-last-stored-link)
