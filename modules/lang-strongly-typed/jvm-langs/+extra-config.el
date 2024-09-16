;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! flycheck-kotlin
  :after kotlin-mode
  :hook (kotlin-mode . flycheck-kotlin-setup)
  )

(use-package! ob-kotlin
  :defer t
  )

(use-package! lsp-java
  :after lsp-mode
  :preface
  (setq lsp-java-workspace-dir (concat doom-data-dir "java-workspace"))
  :config
  (setq lsp-jt-root (concat lsp-java-server-install-dir "java-test/server/")
        dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar")
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
;;; +extra-config.el ends here
