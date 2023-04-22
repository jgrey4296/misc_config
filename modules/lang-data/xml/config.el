;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: September 15, 2022
;; Modified: September 15, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(load! "+vars")
(load! "+funcs")
(load! "+xml-utils")
(after! (jg-bindings-total jg-dired nxml-mode)
  (load! "+bindings")
  )


(use-package! mhtml-mode :defer t)

(use-package! nxml-mode
  :defer t
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  (setq-hook! 'nxml-mode-hook tab-width nxml-child-indent)
  (add-hook! 'nxml-mode-hook 'hs-minor-mode)
  )

(when (modulep! +lsp)
  (add-hook! '(nxml-mode-local-vars-hook html-mode-local-vars-hook) :append #'lsp!))

(when (modulep! +tree-sitter)
  (add-hook! '(html-mode-local-vars-hook
               mhtml-mode-local-vars-hook)
               :append #'tree-sitter!))
;;; config.el ends here
