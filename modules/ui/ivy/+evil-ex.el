;;; +evil-ex.el -*- lexical-binding: t; -*-


(evil-ex-define-cmd "pg[rep]"   #'+ivy:project-search)
(evil-ex-define-cmd "pg[grep]d" #'+ivy:project-search-from-cwd)
