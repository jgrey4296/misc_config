;;; +eglot.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! eglot
  :commands (eglot eglot-ensure)
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :config
  (add-to-list 'doom-debug-variables '(eglot-events-buffer-size . 0))

  )

(use-package! flycheck-eglot
  :after eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  )

(spec-handling-new! eglot eglot-server-programs :loop 'collect
                    (cons key val)
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
