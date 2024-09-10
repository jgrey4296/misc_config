;;; +babel.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! ob
  :defer t
  :config

  ;; babel
  (advice-add 'org-babel-exp-src-block              :before      #'+org--export-lazy-load-library-a)
  (advice-add 'org-babel-confirm-evaluate           :after-while #'+org--babel-lazy-load-library-a)
  (advice-add 'ob-async-org-babel-execute-src-block :around      #'+org-babel-disable-async-maybe-a)
  (advice-add 'org-src--get-lang-mode               :before      #'+org--src-lazy-load-library-a)
  (advice-add 'org-src--edit-element                :around      #'+org-inhibit-mode-hooks-a)
  (advice-add 'org-babel-do-load-languages          :override    #'ignore)
  (advice-add 'org-babel-tangle                     :around      #'+org--dont-trigger-save-hooks-a)

  ;; hooks
  (add-hook 'org-babel-after-execute-hook #'+org-redisplay-inline-images-in-babel-result-h)

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
;;; +babel.el ends here
