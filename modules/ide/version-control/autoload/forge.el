;;; forge.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +magit--forge-get-repository-lazily-a (&rest _)
  "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."

  (file-executable-p emacsql-sqlite-executable))

;;;###autoload
(defun +magit--forge-build-binary-lazily-a (&rest _)
  "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
  (unless (file-executable-p emacsql-sqlite-executable)
    (emacsql-sqlite-compile 2)
    (if (not (file-executable-p emacsql-sqlite-executable))
        (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                         "See *Compile-Log* buffer for details"))
      ;; HACK Due to changes upstream, forge doesn't initialize completely if
      ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
      ;;      manually after installing it.
      (setq forge--sqlite-available-p t)
      (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
      (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
      (after! forge-topic
        (dolist (hook forge-bug-reference-hooks)
          (add-hook hook #'forge-bug-reference-setup))))))



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
;;; forge.el ends here
