;;; +export.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! ox
  :defer t
  :config

  (advice-add 'org-export-inline-image-p            :override #'+jg-org-inline-image-override)
  (advice-add 'org-export-to-file                   :around #'+org--dont-trigger-save-hooks-a)
  (advice-add 'org-export-to-file                   :around #'+org--fix-async-export-a)
  (advice-add 'org-export-as                        :around #'+org--fix-async-export-a)
  )

(use-package! ox-epub
  :after org
  )

(use-package! ox-pandoc
  :when (executable-find "pandoc")
  :after ox
  :init
  (add-to-list 'org-export-backends 'pandoc)
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (variable . "revealjs-url=https://revealjs.com")
          )
        )
  )

(setq org-export-with-smart-quotes t
    org-html-validation-link nil
    org-latex-prefer-user-labels t)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +export.el ends here