;;; +csound.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! csound-mode
  :defer t
  :config
  (after! librarian
    (add-to-list 'librarian-active-on-modes 'csound-mode)
    )

  (add-hook! 'csound-mode-hook
             #'librarian-insert-minor-mode
             )


  )

(speckler-add! auto-modes ()
  '(csound
    ("\\.csd\\'" . csound-mode)
    ("\\.orc\\'" . csound-mode)
    ("\\.sco\\'" . csound-mode)
    )
  )
(speckler-add! file-templates ()
  '(csound
    ("\\.csd\\'" :trigger "__combined"  :mode csound-mode)
    ("\\.orc\\'" :trigger "__orchestra" :mode csound-mode)
    ("\\.sco\\'" :trigger "__score"     :mode csound-mode)
    )
  )
(speckler-add! librarian-regular ()
  (csound-mode
   ("Csound Manual" . "https://csound.com/docs/manual/PartOverview.html")
   ("Csound tutorial" . "http://www.csounds.com/toots/index.html")
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
;;; +csound.el ends here
