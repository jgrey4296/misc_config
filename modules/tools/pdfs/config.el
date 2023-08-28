;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 02, 2023
;; Modified: April 02, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(local-load! "+vars")
(defer-load! (jg-bindings-total jg-dired-bindings) "+bindings")

(use-package! pdf-meta-mode
  :commands (pdf-meta-mode pdf-meta-extract-info pdf-meta-split pdf-meta-join pdf-meta-attach pdf-meta-detach)
  )

(use-package! org-pdftools
  :commands org-pdftools-export
  :after org
  :init
    (org-link-set-parameters (or (bound-and-true-p org-pdftools-link-prefix) "pdf")
                             :follow   (+org--pdftools-link-handler #'org-pdftools-open)
                             :complete (+org--pdftools-link-handler #'org-pdftools-complete-link)
                             :store    (+org--pdftools-link-handler #'org-pdftools-store-link)
                             :export   (+org--pdftools-link-handler #'org-pdftools-export))
    (add-hook! 'org-open-link-functions #'+org-open-legacy-pdf-links-fn)

    )


;;; config.el ends here
