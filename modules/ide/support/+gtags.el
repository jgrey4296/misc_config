;;; +gtags.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! counsel-gtags :defer t)

(use-package! helm-gtags :defer t)

(speckler-add! lib-env ()
  '(gtags
    :setup    nil
    :start    nil
    :stop     nil
    :teardown nil
    )
  )

(speckler-add! lookup-handler ()
  `(gtags
    :definition          nil
    :declaration         nil
    :references          nil
    :documentation       nil
    :implementations     nil
    :type-definition     nil
    )
  )

(speckler-add! popup ()
  '(popup
    nil
    )
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
;;; +gtags.el ends here
