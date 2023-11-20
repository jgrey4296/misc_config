;;; transient-compile.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

(progn ;; define the calls
  ;; (transient-make-call! {name} "p" "Desc" (call) )
  ;; (transient-make-int-call! magit-todos         "t"   "Todos"             :transient nil #'magit-todos-list)
  ;; (transient-make-var-toggle! auto-balance evil-auto-balance-windows "Auto-Balance Windows" "B")
  )

;; Create the transient

;;;###autoload (autoload 'transient-latex-compile "lang-text/latex/autoload/transient-compile.el" nil t)
(transient-define-prefix transient-latex-compile ()
  ""
  [:description (lambda () "")
                []
   ]
  ["Options"

   ]
  ["Run"

   ]
  transient-quit!
  )



;;-- Footer
;; Copyright (C) 2023 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 20, 2023
;; Modified:   November 20, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; transient-compile.el ends here
