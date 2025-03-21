;;; +extra-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


(use-package! company-gtags
  :when (modulep! :ide company)
  )

(use-package! company-keywords
  :when (modulep! :ide company)
)

(use-package! ivy-xref
  :when (modulep! :ui ivy)
  :config
  )

(use-package! dash-docs
  :defer t
  :config
  (setq dash-docs-enable-debugging init-file-debug
        dash-docs-docsets-path (expand-file-name "~/_cache_/docsets")
        dash-docs-min-length 2
        dash-docs-browser-func #'browse-url
        )

  (require 'counsel-dash nil t)
)

(use-package! browse-url :defer t)

(use-package! dumb-jump
  :disabled
  :after xref
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
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
