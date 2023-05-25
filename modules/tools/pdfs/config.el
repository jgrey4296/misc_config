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


(load! "+vars")
(after! (jg-bindings-total jg-dired-bindings)
  (load! "+bindings")
  )

(use-package! pdf-meta-mode
  :commands (pdf-meta-mode pdf-meta-extract-info pdf-meta-split pdf-meta-join pdf-meta-attach pdf-meta-detach)
  :mode ("\\.pdf.info\\'" . pdf-meta-mode)
  )

;;; config.el ends here
