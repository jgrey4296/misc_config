;;; +popup.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! rustic-popup
  :after rustic
  :config
  (setq rustic-popup-commands
        '((?b "build"    build)
          (?f "fmt"      fmt)
          (?r "run"      run)
          (?c "clippy"   clippy)
          (?o "outdated" outdated)
          (?e "clean"    clean)
          (?k "check"    check)
          (?t "test"     test)
          (?d "doc"      doc)
          (?l "list commands" list)
          ;; alldocs
          ;; do-release
          ;; jg
          )
        )
  )

(defun rustic-cargo-list (&optional arg)
  "List installed cargo commands"
  (interactive "P")
  (rustic-compilation-process-live)
  (let* ((command (flatten-list (list (rustic-cargo-bin) "--list")))
         (buf rustic-compilation-buffer-name)
         (proc rustic-compilation-buffer-name)
         (mode 'rustic-cargo-plain-run-mode)
         )

    (rustic-compilation command
                        (list :buffer buf :process proc :mode mode))
    )
  )

(speckler-add! evil-initial ()
  '(rustic-popup-mode insert)
  )

(speckler-add! popup ()
  '(rustic
    ("^\\*rustic-compilation"     :vslot -1)
    ("^\\*cargo-"                 :vslot -1)
    ("^rustic-popup-buffer"       :vslot -1)
    ("^rustic-popup-help-buffer"  :vslot -1)
    )
  )
;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2025
;; Modified:   September 10, 2025
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
;;; +popup.el ends here
