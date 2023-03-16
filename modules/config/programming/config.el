;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 15, 2023
;; Modified: March 15, 2023
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
(load! "+advice")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(use-package! cedet)
(use-package! semantic
  :defer t
  :config
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'semantic-highlight-func-mode)
  (add-to-list 'semantic-new-buffer-setup-functions
               '(emacs-lisp-mode . semantic-default-elisp-setup))
  ;; TODO setup semantic more, add helm etc
  )

;;; config.el ends here
