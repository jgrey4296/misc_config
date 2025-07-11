;;; project-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'macro-tools--transient)

;; -- General

;; -- Locations
(transient-call! proj-root ()
  ""
  :key "`"
  :desc (macro-tools--transient-simple-fmt "Project Root" "`")
  (find-file (projectile-project-root))
  )
(transient-call! proj-sidebar ()
  ""
  :key "s"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Sidebar" "s")
  :transient nil
  #'+jg-ui-tree/open
  )
(transient-call! recent-files  ()
  "Recent Files"
  :key "r"
  :desc (macro-tools--transient-simple-fmt "Project Recent Files" "r")
  :transient nil
  (projectile-recentf)
  )

;; -- Projects
(transient-call! proj-browse ()
  ""
  :key ">"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Projects" ">")
  :transient t
  #'+jg-projects-switch-to
  )

(transient-call! proj-switch ()
  ""
  :key "="
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project Switch" "=")
  :transient t
  #'projectile-switch-project
  )
(transient-call! proj-add ()
  ""
  :key "+"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Add Project" "+")
  #'projectile-add-known-project
  )
(transient-call! proj-clear-cache ()
  ""
  :key "-"
  :desc (macro-tools--transient-simple-fmt "Project Clear Cache" "-")
  (projectile-invalidate-cache nil)
  (projectile-clear-known-projects)
  )

;; -- Project Management
(transient-call! debug-project-type ()
  ""
  :key "?"
  :desc (macro-tools--transient-simple-fmt "Debug Project Type" "?")
  (+jg-projects-detect-type)
  )

;; -- Project Actions
(transient-call! proj-cmd ()
  ""
  :key "!"
  :desc (macro-tools--transient-simple-fmt "Project Cmd" "!")
  :interactive t
  :transient nil
  #'projectile-run-shell-command-in-root
  )
(transient-call! proj-config ()
  ""
  :key "e"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project Editor Config" "e")
  :transient nil
  #'editorconfig-find-current-editorconfig
  )
(transient-call! proj-configure ()
  ""
  :key "c"
  :desc (macro-tools--transient-simple-fmt "Project Config" "c")
  :transient nil
  :interactive t
  #'+jg-projects-open-configs
  )
(transient-call! proj-dir-locals ()
  ""
  :key "l"
  :desc (macro-tools--transient-simple-fmt "Project Locals" "l")
  :interactive t
  :transient nil
  #'projectile-edit-dir-locals
  )
(transient-call! proj-file  ()
  ""
  :key "-ff"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project File" "-ff")
  :transient nil
  #'projectile-find-file
  )
(transient-call! proj-kill ()
  ""
  :key "K"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Kill Project Buffers" "K")
  #'projectile-kill-buffers
  )
(transient-call! proj-recent ()
  ""
  :key "-fr"
  :desc (macro-tools--transient-simple-fmt "Project Recent" "-fr")
  :interactive t
  :transient nil
  #'projectile-recentf
  )
(transient-call! proj-regexp ()
  ""
  :key "r"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Replace Regexp in Project" "r")
  :transient nil
  #'zimmerframe-replace-regexp
  )
(transient-call! proj-related ()
  ""
  :key "R"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project Related" "R")
  :transient nil
  #'+jg-projects-find-related
  )
(transient-call! proj-replace ()
  ""
  :key "s"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Replace String in Project" "s")
  :transient nil
  #'projectile-replace
  )
(transient-call! proj-save ()
  ""
  :key "S"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Save Project Buffers" "S")
  #'projectile-save-project-buffers
  )
(transient-call! proj-scratch ()
  ""
  :key "x"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project Scratch" "x")
  :transient nil
  #'+jg-ui-open-scratch-buffer
  )
(transient-call! proj-shell ()
  ""
  :key "'"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project Shell" "'")
  :transient nil
  #'projectile-run-shell
  )
(transient-call! proj-symbol ()
  ""
  :key "."
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Project Symbol" ".")
  :transient nil
  #'+jg-workspaces-search-project-for-symbol-at-point
  )

;; --
(defun jg-workspace--build-project-transient-groups  ()
  (transient-subgroup! transient-project ()
    "Manage Project"
    :key "p"
    :rows t
    :desc "+Manage Project"
     [
     ["Project Shell and Cmds"
      (transient-macro-call-proj-cmd)
      (transient-macro-call-proj-shell)
      ]
     ["View"
      (transient-macro-call-magit-todos)
      (transient-macro-call-proj-root)
      (transient-macro-call-proj-symbol)
      ]
     ]
     ["Change Project-Wide"
      (transient-macro-call-proj-replace)
      (transient-macro-call-proj-regexp)
      ]
     [
      ["Files"
       (transient-macro-call-proj-file)
       (transient-macro-call-proj-recent)
       (transient-macro-call-proj-related)
       (transient-macro-call-proj-kill)
       (transient-macro-call-proj-save)
       ]
      ["Specific Files"
       (transient-macro-call-proj-scratch)
       (transient-macro-call-proj-config)
       (transient-macro-call-proj-dir-locals)
       (transient-macro-call-proj-configure)
       ]
      ]
    )
)

;;;###autoload
(defun jg-workspace-build-project-transient  ()
  (jg-workspace--build-project-transient-groups)

  (transient-append-suffix 'workspace-control-transient '(-2)
    [""
     ["|| Projects ||"
      transient-project
      ]
     [" | Settings |"
      (transient-macro-call-debug-project-type)
      (transient-macro-call-proj-clear-cache)
      (transient-macro-call-proj-add)
      (transient-macro-call-proj-switch)
      ]
     ]
    )

  (transient-append-suffix 'workspace-control-transient "t"   '(transient-macro-call-proj-sidebar))

  (transient-append-suffix 'workspace-control-transient '(0 1)
    [""
     (transient-macro-call-proj-root)
     (transient-macro-call-proj-browse)
     (transient-macro-call-recent-files)
     (transient-macro-call-proj-configure)
     ]
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    August 22, 2024
;; Modified:   August 22, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; project-transient.el ends here
