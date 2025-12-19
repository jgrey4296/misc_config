;;; +eglot.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(advice-add 'eglot--managed-mode             :around #'+lsp--defer-server-shutdown-a)

(use-package! eglot
  :commands (eglot eglot-ensure)
  :config

  )

(use-package! flycheck-eglot
  :after eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  )

(setq eglot-sync-connect 1
      eglot-connect-timeout 10
      eglot-autoshutdown t
      eglot-send-changes-idle-time 0.5
      ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
      ;;      its popup rule causes eglot to steal focus too often.
      eglot-auto-display-help-buffer nil
      eglot-stay-out-of '(flymake)
      )

(speckler-new! eglot (key val)
  "Register eglot servers"
  :target eglot-server-programs
  :loop 'collect
  (cons key val)
  )

(speckler-add! lib-env ()
  '(eglot
    :start #'(lambda (state &rest rest) (add-hook 'python-mode-hook #'eglot-ensure))
    :stop  #'(lambda (state) (remove-hook 'python-mode-hook #'eglot-ensure))
    )
  )

(speckler-add! lookup-handler ()
  `(eglot--managed-mode
    :definition          xref-find-definitions
    :references          xref-find-references
    :implementations     eglot-find-implementation
    :type-definition     eglot-find-typeDefinition
    :documentation       +eglot-lookup-documentation
    )
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
;;; +eglot.el ends here
