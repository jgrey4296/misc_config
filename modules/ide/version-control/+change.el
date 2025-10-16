;;; +change.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


(use-package! markdown-changelog
  :defer t
  )

(use-package! git-cliff
  :defer t
  )

(use-package! conventional-changelog
  :defer t
  )

;;  --------------------------------------------------
(speckler-setq! changelog ()
  change-log-default-name "CHANGELOG.md"
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    October 17, 2025
;; Modified:   October 17, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +change.el ends here
