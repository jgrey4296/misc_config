;;; +snippet-fix.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 04, 2022
;; Modified: March 04, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/johngrey/+snippet-fix
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(defun +jg-completion-snippet--completing-read-uuid (prompt all-snippets &rest args)
  (let* ((snippet-data (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                                   (hash-table-values yas--tables)
                                                                                 (yas--get-snippet-tables)))

                                unless (null (yas--template-load-file tpl))
                                for txt = (format "%-25s%-30s%s"
                                                  (yas--template-key tpl)
                                                  (yas--template-name tpl)
                                                  (abbreviate-file-name (yas--template-load-file tpl)))
                                collect
                                `(,txt . ,(yas--template-uuid tpl))))
        (selected-value (apply #'completing-read prompt snippet-data args)))
  (alist-get selected-value snippet-data nil nil 'equal)))


;;; +snippet-fix.el ends here
