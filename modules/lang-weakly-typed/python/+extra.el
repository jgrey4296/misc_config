;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(advice-add 'pip-requirements-complete-at-point :before #'+python--init-completion-a)
(advice-add 'pip-requirements-mode              :around #'+python--inhibit-pip-requirements-fetch-packages-a)

(use-package! company-anaconda
  :after anaconda-mode
  :commands 'company-anaconda
  :config
  (speckler-add! company ()
    '(anaconda-mode (:mode company-anaconda))
    )
  )

(use-package! company-jedi
  :when (modulep! :ide company)
  :defer t
  )

(use-package! py-isort
  :commands py-isort-buffer
  )

(use-package! pyimport
  :after python-mode
  )

(use-package! pydoc
  :after python-mode
  ;; cmds pydoc, pydoc-at-point, pydoc-browse, pydoc-browse-kill
  )

(use-package! pip-requirements
  :commands pip-requirements-mode
  )

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
;;; +extra-config.el ends here
