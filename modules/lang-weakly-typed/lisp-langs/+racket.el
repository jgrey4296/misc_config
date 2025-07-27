;;; +racket.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! racket-mode
  :commands (racket-mode)
  :config
  (add-hook! 'racket-mode-hook
                 #'rainbow-delimiters-mode
                 #'highlight-quoted-mode)

  (add-hook 'racket-mode-local-vars-hook #'racket-xp-mode)
  ;; Both flycheck and racket-xp produce error popups, but racket-xp's are
  ;; higher quality so disable flycheck's:
  (add-hook! 'racket-xp-mode-hook
    (defun +racket-disable-flycheck-h ()
      (cl-pushnew 'racket flycheck-disabled-checkers)
      )
    )
  (add-hook 'racket-mode-hook #'racket-smart-open-bracket-mode)

  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 27, 2025
;; Modified:   July 27, 2025
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
;;; +racket.el ends here
