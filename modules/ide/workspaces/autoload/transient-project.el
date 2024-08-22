;;; project-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

;; TODO move compile to eval module
;;
(transient-make-call!     proj-clear-cache    "C"   "Project Clear Cache" (projectile-invalidate-cache nil))
(transient-make-int-call! proj-sidebar        "s"   "Sidebar"            :transient nil #'+jg-ui-tree/open)
(progn
  (transient-make-call! proj-root            "`"   "Project Root" (find-file (projectile-project-root)))

  (transient-make-int-call! proj-add         "a"   "Add Project"               #'projectile-add-known-project)
  (transient-make-int-call! proj-clean       "c"   "Clean Project"             #'+jg-projects-clean)
  (transient-make-call! proj-clear-known     "D"   "Clear Project List Cache"  (projectile-clear-known-projects) (clrhash projectile-project-root-cache))

  (transient-make-int-call! proj-cmd         "!"   "Project Cmd"               :transient nil #'projectile-run-shell-command-in-root)
  (transient-make-int-call! proj-compile     "c"   "Compile Project"           :transient nil #'projectile-compile-project)
  (transient-make-int-call! proj-config      "e"   "Project Editor Config"     :transient nil #'editorconfig-find-current-editorconfig)
  (transient-make-int-call! proj-configure   "c"   "Project Config"            :transient nil #'+jg-projects-open-configs)
  (transient-make-int-call! proj-dir-locals  "l"   "Project Locals"            :transient nil #'projectile-edit-dir-locals)
  (transient-make-int-call! proj-discover    "d"   "Discover Projects"          #'+default/discover-projects)
  (transient-make-int-call! proj-file        "-ff"   "Project File"              :transient nil #'projectile-find-file)
  (transient-make-int-call! proj-finder      "F"   "Reveal in Finder"          :transient nil #'+macos/reveal-project-in-finder)
  (transient-make-int-call! proj-kill        "K"   "Kill Project Buffers"      #'projectile-kill-buffers)
  (transient-make-int-call! proj-recent      "-fr"   "Project Recent"                 :transient nil #'projectile-recentf)
  (transient-make-int-call! proj-regexp      "r"   "Replace Regexp in Project"        :transient nil #'zimmerframe-replace-regexp)
  (transient-make-int-call! proj-related     "R"   "Project Related"                  :transient nil #'+jg-projects-find-related)
  (transient-make-int-call! proj-replace     "s"   "Replace String in Project"        :transient nil #'projectile-replace)
  (transient-make-int-call! proj-run         "r"   "Run Project"               #'projectile-run-project)
  (transient-make-int-call! proj-save        "S"   "Save Project Buffers"      #'projectile-save-project-buffers)
  (transient-make-int-call! proj-scratch     "x"   "Project Scratch"           :transient nil #'+jg-ui-open-scratch-buffer)
  (transient-make-int-call! proj-shell       "'"   "Project Shell"             :transient nil #'projectile-run-shell)
  (transient-make-int-call! proj-switch      "p"   "Project Switch"            :transient nil #'projectile-switch-project)
  (transient-make-int-call! proj-symbol      "."   "Project Symbol"            :transient nil #'+default/search-project-for-symbol-at-point)
  (transient-make-int-call! proj-test        "t"   "Test Project"              #'projectile-test-project)
  (transient-make-int-call! proj-browse      ">"   "Browse Projects"           #'doom/browse-in-other-project)
  )

(transient-make-subgroup! transient-project-actions "a"
                          ""
                          :desc "+Project Actions"
                          ["Project Actions"
                           (transient-macro-call-proj-run)
                           (transient-macro-call-proj-compile)
                           (transient-macro-call-proj-test)
                           (transient-macro-call-proj-clean)
                           (transient-macro-call-proj-finder)
                           (transient-macro-call-proj-clear-cache)
                           ]
                          )
(transient-make-subgroup! transient-project "p"
                          ""
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
(transient-make-subgroup! transient-all-projects "P"
  ""
  :desc "+Manage All Projects"
  [
   ["Project Lists"
    (transient-macro-call-proj-browse)
    (transient-macro-call-proj-switch)
    ]
   [" "
    (transient-macro-call-proj-add)
    (transient-macro-call-proj-discover)
    (transient-macro-call-proj-clear-known)
    ]
   ]
  )


(defun +jg-workspace-add-project-transient ()
  (transient-append-suffix 'workspace-control-transient '(2 0)
     ["Project"
      transient-all-projects
      transient-project
      transient-project-actions
      ]
    )

  (transient-append-suffix 'workspace-control-transient '(3 0)
     ["Locs" ;; column
      (transient-macro-call-proj-sidebar)
      (transient-macro-call-recent-files)
      (transient-macro-call-zimmerframe-next)
      (transient-macro-call-zimmerframe-prev)
      ]
     )
  (transient-append-suffix 'workspace-control-transient '(3 -1)
     ["Settings" ;; column
      (transient-macro-call-debug-project-type)
      (transient-macro-call-proj-clear-cache)
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
