;;; config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


(use-package! timeline-mode)
(use-package! quote-mode)
(use-package! definition-lookup-mode)

(speckler-add! auto-modes ()
  '(quotes
    ("\\.quote\\'"      . quote-mode)
    ("\\.timeline\\'"   . timeline-mode)
    ("\\.definition\\'" . definition-mode)
    )
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 31, 2025
;; Modified:   January 31, 2025
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
;;; config.el ends here
