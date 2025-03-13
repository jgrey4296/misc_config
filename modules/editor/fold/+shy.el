;;; +shy.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! code-shy-minor-mode
  :hook (prog-mode . code-shy-minor-mode)
  :hook (toml-mode . code-shy-minor-mode)
  :init
  (setq code-shy-fold-patterns (list "%s-- %s %s" "%s-- %s %s"))
)

(setq code-shy-exclusions '(helm-major-mode
                            ivy-mode
                            minibuffer-mode
                            dired-mode
                            fundamental-mode
                            rst-mode
                            magit-status-mode
                            helpful-mode
                            shell-mode
                            ))

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 09, 2025
;; Modified:   February 09, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +shy.el ends here
