;;; +transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! transient)

(after! transient
  ;; Default hooks in config/ui
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-hooks-transient 90)
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-debugs-transient)
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-guides-transient)
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-nav-transient)
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-visuals-transient)
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-wrap-transient)

  (jg-ui-transient-toggles-builder)
  (add-hook 'speckler-hook #'jg-ui-transient-toggles-builder)
  (provide 'transient-toggles)
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 04, 2024
;; Modified:   December 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +transient.el ends here
