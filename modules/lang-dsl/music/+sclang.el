;;; +sclang.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! sclang
  :commands sclang-mode
  :config
  (setq-default
   sclang-eval-line-forward nil
   sclang-show-workspace-on-startup nil
   sclang-auto-scroll-post-buffer t
   sclang-program "sclang"
   sclang-rtf-editor-program "emacs"
   sclang-udp-port 57120
   sclang-runtime-directory (expand-file-name "~/.cache/sclang/")
   sclang-library-configuration-file (expand-file-name "modules/jg-music-layer/sclang.yaml" doom-user-dir)
   )

  )

(use-package! sclang-extensions
  :after sclang
  )

;; --------------------------------------------------

(speckler-add! librarian-regular ()
  (sclang-mode
   ("Supercollider docs" . "http://doc.sccode.org/")
   )
  )

(speckler-add! auto-modes ()
  '(sclang
    ("\\.scd\\'" . sclang-mode)
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    October 15, 2025
;; Modified:   October 15, 2025
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
;;; +sclang.el ends here
