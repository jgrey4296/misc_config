;;; project-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'macro-tools--transient)

;; TODO move compile to eval module
;;
(transient-call! recent-files  ()
  "Recent Files"
  :key "r"
  :desc (transient-simple-formatter "Project Recent Files" "r")
  :transient nil
  (projectile-recentf)
  )
(transient-call! debug-project-type ()
  ""
  :key "?"
  :desc (transient-simple-formatter "Debug Project Type" "?")
  )
(transient-call! proj-clear-cache ()
  ""
  :key "C"
  :desc (transient-simple-formatter "Project Clear Cache" "C")
  )
(transient-call! proj-sidebar ()
  ""
  :key "s"
  :interactive t
  :desc (transient-simple-formatter "Sidebar" "s")
  :transient nil
  )
(transient-call! proj-root ()
  ""
  :key "`"
  :desc (transient-simple-formatter "Project Root" "`")
  (find-file (projectile-project-root))
  )
(transient-call! proj-add ()
  ""
  :key "a"
  :interactive t
  :desc (transient-simple-formatter "Add Project" "a")
  #'projectile-add-known-project
  )
(transient-call! proj-clean ()
  ""
  :key "c"
  :desc (transient-simple-formatter "Clean Project" "c")
  :interactive t
  #'+jg-projects-clean
  )
(transient-call! proj-clear-known ()
  ""
  :key "D"
  :desc (transient-simple-formatter "Clear Project List Cache" "D")
  (projectile-clear-known-projects)
  (clrhash projectile-project-root-cache)
  )
(transient-call! proj-cmd ()
  ""
  :key "!"
  :desc (transient-simple-formatter "Project Cmd" "!")
  :interactive t
  :transient nil
  #'projectile-run-shell-command-in-root
  )
(transient-call! proj-compile ()
  ""
  :key "c"
  :desc (transient-simple-formatter "Compile Project" "c")
  :transient nil
  #'projectile-compile-project
  )
(transient-call! proj-config ()
  ""
  :key "e"
  :interactive t
  :desc (transient-simple-formatter "Project Editor Config" "e")
  :transient nil
  #'editorconfig-find-current-editorconfig
  )
(transient-call! proj-configure ()
  ""
  :key "c"
  :desc (transient-simple-formatter "Project Config" "c")
  :transient nil
  :interactive t
  #'+jg-projects-open-configs
  )
(transient-call! proj-dir-locals ()
  ""
  :key "l"
  :desc (transient-simple-formatter "Project Locals" "l")
  :interactive t
  :transient nil
  #'projectile-edit-dir-locals
  )
(transient-call! proj-discover ()
  ""
  :key "d"
  :interactive t
  :desc (transient-simple-formatter "Discover Projects" "d")
  #'+default/discover-projects
  )
(transient-call! proj-file  ()
  ""
  :key "-ff"
  :interactive t
  :desc (transient-simple-formatter "Project File" "-ff")
  :transient nil
  )
(transient-call! proj-finder ()
  ""
  :key "F"
  :interactive t
  :desc (transient-simple-formatter "Reveal in Finder" "F")
  :transient nil
  )
(transient-call! proj-kill ()
  ""
  :key "K"
  :interactive t
  :desc (transient-simple-formatter "Kill Project Buffers" "K")
  )
(transient-call! proj-recent ()
  ""
  :key "-fr"
  :desc (transient-simple-formatter "Project Recent" "-fr")
  :interactive t
  :transient nil
  )
(transient-call! proj-regexp ()
  ""
  :key "r"
  :interactive t
  :desc (transient-simple-formatter "Replace Regexp in Project" "r")
  :transient nil
  )
(transient-call! proj-related ()
  ""
  :key "R"
  :interactive t
  :desc (transient-simple-formatter "Project Related" "R")
  :transient nil
  )
(transient-call! proj-replace ()
  ""
  :key "s"
  :interactive t
  :desc (transient-simple-formatter "Replace String in Project" "s")
  :transient nil
  )
(transient-call! proj-run ()
  ""
  :key "r"
  :interactive t
  :desc (transient-simple-formatter "Run Project" "r")
  )
(transient-call! proj-save ()
  ""
  :key "S"
  :interactive t
  :desc (transient-simple-formatter "Save Project Buffers" "S")
  )
(transient-call! proj-scratch ()
  ""
  :key "x"
  :interactive t
  :desc (transient-simple-formatter "Project Scratch" "x")
  :transient nil
  )
(transient-call! proj-shell ()
  ""
  :key "'"
  :interactive t
  :desc (transient-simple-formatter "Project Shell" "'")
  :transient nil
  )
(transient-call! proj-switch ()
  ""
  :key "p"
  :interactive t
  :desc (transient-simple-formatter "Project Switch" "p")
  :transient nil
  )
(transient-call! proj-symbol ()
  ""
  :key "."
  :interactive t
  :desc (transient-simple-formatter "Project Symbol" ".")
  :transient nil
  )
(transient-call! proj-test ()
  ""
  :key "t"
  :interactive t
  :desc (transient-simple-formatter "Test Project" "t")
  )
(transient-call! proj-browse ()
  ""
  :key ">"
  :interactive t
  :desc (transient-simple-formatter "Browse Projects" ">")
  )


(defun jg-workspace--build-project-transient-groups  ()
  (transient-subgroup! transient-project-actions ()
    ""
    :key "a"
    :desc "+Project Actions"
    (transient-macro-call-proj-run)
    (transient-macro-call-proj-compile)
    (transient-macro-call-proj-test)
    (transient-macro-call-proj-clean)
    (transient-macro-call-proj-finder)
    (transient-macro-call-proj-clear-cache)
    )
  (transient-subgroup! transient-project ()
    ""
    :key "p"
    :desc "+Manage Project"
    [
     ["Change Project-Wide"
      (transient-macro-call-proj-replace)
      (transient-macro-call-proj-regexp)
      ]
     ]
    [
     ["View"
      (transient-macro-call-magit-todos)
      (transient-macro-call-proj-root)
      (transient-macro-call-proj-symbol)
      ]
     ["Project Shell and Cmds"
      (transient-macro-call-proj-cmd)
      (transient-macro-call-proj-shell)
      ]
     ]
    [
     ["Specific Files"
      (transient-macro-call-proj-scratch)
      (transient-macro-call-proj-config)
      (transient-macro-call-proj-dir-locals)
      (transient-macro-call-proj-configure)
      ]
     ["Files"
      (transient-macro-call-proj-file)
      (transient-macro-call-proj-related)
      (transient-macro-call-proj-recent)
      (transient-macro-call-proj-kill)
      (transient-macro-call-proj-save)
      ]
     ]
    )
  (transient-subgroup! transient-all-projects ()
    ""
    :key "P"
    :desc "+Manage All Projects"
    ["Project Lists"
     (transient-macro-call-proj-browse)
     (transient-macro-call-proj-switch)
     ]
    [" "
     (transient-macro-call-proj-add)
     (transient-macro-call-proj-discover)
     (transient-macro-call-proj-clear-known)
     ]
    )
  )

;;;###autoload
(defun jg-workspace-build-project-transient  ()
  (jg-workspace--build-project-transient-groups)

  (transient-append-suffix 'workspace-control-transient '(-2)
    [""
     ["|| Projects ||"
      transient-all-projects
      transient-project
      transient-project-actions
      ]
     [" | Locations |"
      (transient-macro-call-proj-sidebar)
      (transient-macro-call-recent-files)
      (transient-macro-call-zimmerframe-next)
      (transient-macro-call-zimmerframe-prev)
      ]
     [" | Settings |"
      (transient-macro-call-debug-project-type)
      (transient-macro-call-proj-clear-cache)
      ]
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
