;;; +rules.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


(use-package! soar-mode
  :defer t
  :config

  (add-hook! 'soar-mode-hook
             #'librarian-insert-minor-mode
             )

  )

(use-package! clips-mode       :defer t)


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 15, 2025
;; Modified:   September 15, 2025
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
;;; +rules.el ends here
