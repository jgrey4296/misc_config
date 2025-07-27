;;; +realgud.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(advice-add 'realgud:run-process :override #'+debugger--realgud-open-in-other-window-a)
(advice-add 'realgud:terminate   :after #'+debugger--cleanup-after-realgud-a)

(defvar +debugger--realgud-alist
  '((realgud:bashdb    :modes (sh-mode))
    (realgud:gdb)
    (realgud:gub       :modes (go-mode))
    (realgud:kshdb     :modes (sh-mode))
    (realgud:pdb       :modes (python-mode))
    (realgud:perldb    :modes (perl-mode perl6-mode))
    (realgud:rdebug    :modes (ruby-mode))
    (realgud:remake)
    (realgud:trepan    :modes (perl-mode perl6-mode))
    (realgud:trepan2   :modes (python-mode))
    (realgud:trepan3k  :modes (python-mode))
    (realgud:trepanjs  :modes (javascript-mode js2-mode js3-mode))
    (realgud:trepanpl  :modes (perl-mode perl6-mode raku-mode))
    (realgud:zshdb     :modes (sh-mode)))
  )

(use-package! realgud
  :defer t
  :init
  ;; Realgud doesn't generate its autoloads properly so we do it ourselves
  (dolist (debugger +debugger--realgud-alist)
    (autoload (car debugger)
      (if-let (sym (plist-get (cdr debugger) :package))
          (symbol-name sym)
        "realgud")
      nil t))

  :config

  )

;; --------------------------------------------------

(use-package! realgud-lldb
  :after realgud)

(use-package! realgud-ipdb
  :after realgud)

(use-package! realgud-jdb
  :after realgud)

(use-package! realgud-node-inspect
  :after realgud)

(use-package! realgud-node-debug
  :after realgud)

;; --------------------------------------------------

;; (speckler-add! popup ()
;;   '(realgud
;;     )
;;   )

(speckler-add! evil-ex ()
  '(debugger
    ("debug" . #'+debugger/start)
    )
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
;;; +realgud.el ends here
