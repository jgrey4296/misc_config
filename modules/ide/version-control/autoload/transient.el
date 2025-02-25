;;; transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun jg-magit-modules-confirm (verb &optional predicate)
  " Custom module read that can handle multiple selections "
  (let ((magit--refresh-cache (list (cons 0 0)))
        (modules nil))
    (ivy-read
     (format "%s module: " verb)
     (magit-list-module-paths)
     :predicate predicate
     :action #'(lambda (x) (push x modules))
     )
    modules
    ))

;;;###autoload (autoload 'jg-magit-submodule-populate "ide/version-control/autoload/transient" nil t)
(transient-define-suffix jg-magit-submodule-populate (modules args depth)
  "Create MODULES working directories, checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This is the command that actually "initializes" modules.
  ;; A module is initialized when it has a working directory,
  ;; a gitlink, and a .gitmodules entry.
  :class 'magit--git-submodule-suffix
  :description "jg Populate       git submodule update --init [--recursive] [--depth ..]"
  (interactive
   (list (jg-magit-modules-confirm "Populate" 'magit-module-no-worktree-p)
         (magit-submodule-arguments "--recursive" "--no-fetch")
         (magit-submodule-arguments "--depth")
         ))
  (magit-with-toplevel
    (if depth
        (magit-run-git-async "submodule" "update" "--init" args
                             "--depth" (magit-read-number-string "Depth: ")
                             "--" modules)
      (magit-run-git-async "submodule" "update" "--init" args "--" modules)
      )
    )
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 25, 2025
;; Modified:   February 25, 2025
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
;;; transient.el ends here
