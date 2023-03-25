;;; snippet-edit.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: January 24, 2022
;; Modified: January 24, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/johngrey/snippet-edit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; https://github.com/hlissner/doom-emacs/issues/4127
;;  https://github.com/emacs-helm/helm/issues/2453
;;
;;  helm strips text properties by default, so adding text-properties then using completing-read
;;  fails for +snippet--completing-read-uuid in doom emacs.
;;
;;; Code:
(defun +snippet--completing-read-uuid (prompt all-snippets &rest args)
  (let* ((snippet-data (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                                   (hash-table-values yas--tables)
                                                                                 (yas--get-snippet-tables)))

                               for txt = (format "%-25s%-30s%s"
                                                 (yas--template-key tpl)
                                                 (yas--template-name tpl)
                                                 (abbreviate-file-name (yas--template-load-file tpl)))
                               collect
                               `(,txt . ,(yas--template-uuid tpl))))
        (selected-value (apply #'completing-read prompt snippet-data args)))
  (alist-get selected-value snippet-data nil nil 'equal)))



(provide 'snippet-edit)
;;; snippet-edit.el ends here
