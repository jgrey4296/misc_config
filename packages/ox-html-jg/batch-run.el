;;; batch-run.el -*- lexical-binding: t; -*-
;;; https://orgmode.org/manual/HTML-Export.html
(require 'subr-x)
(require 'cl-lib)

(add-to-list 'load-path "/Volumes/documents/github/__configs/packages/ox-html-jg/")

(cl-loop for file in (directory-files "/Volumes/documents/github/_libs/lisp/doom_native/.local/straight/repos" t)
         if (file-directory-p file)
         do
         (add-to-list 'load-path file)
         )

(require 'ox-html-jg)

(add-to-list 'org-export-backends 'html-jg)

;; (message "Load Path: %s" (string-join load-path "\n"))
