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
(progn
  (transient-make-call! recent-files             "r"
                        (transient-simple-formatter "Project Recent Files" "r")
                        :transient nil
                        (projectile-recentf))
  (transient-make-call! debug-project-type       "?"
                        (transient-simple-formatter "Debug Project Type" "?")
                        (+jg-projects-detect-type))
  (transient-make-call!     proj-clear-cache "C"
                            (transient-simple-formatter "Project Clear Cache" "C")
                            (projectile-invalidate-cache nil))
  (transient-make-int-call! proj-sidebar     "s"
                            (transient-simple-formatter "Sidebar" "s")
                            :transient nil
                            #'+jg-ui-tree/open)
  (transient-make-call!     proj-root        "`"
                            (transient-simple-formatter "Project Root" "`")
                            (find-file (projectile-project-root)))
  (transient-make-int-call! proj-add         "a"
                            (transient-simple-formatter "Add Project" "a")
                            #'projectile-add-known-project)
  (transient-make-int-call! proj-clean       "c"
                            (transient-simple-formatter "Clean Project" "c")
                            #'+jg-projects-clean)
  (transient-make-call! proj-clear-known     "D"
                        (transient-simple-formatter "Clear Project List Cache" "D")
                        (projectile-clear-known-projects)
                        (clrhash projectile-project-root-cache))
  (transient-make-int-call! proj-cmd         "!"
                            (transient-simple-formatter "Project Cmd" "!")
                            :transient nil
                            #'projectile-run-shell-command-in-root)
  (transient-make-int-call! proj-compile     "c"
                            (transient-simple-formatter "Compile Project" "c")
                            :transient nil
                            #'projectile-compile-project)
  (transient-make-int-call! proj-config      "e"
                            (transient-simple-formatter "Project Editor Config" "e")
                            :transient nil
                            #'editorconfig-find-current-editorconfig)
  (transient-make-int-call! proj-configure   "c"
                            (transient-simple-formatter "Project Config" "c")
                            :transient nil
                            #'+jg-projects-open-configs)
  (transient-make-int-call! proj-dir-locals  "l"
                            (transient-simple-formatter "Project Locals" "l")
                            :transient nil
                            #'projectile-edit-dir-locals)
  (transient-make-int-call! proj-discover    "d"
                            (transient-simple-formatter "Discover Projects" "d")
                            #'+default/discover-projects)
  (transient-make-int-call! proj-file        "-ff"
                            (transient-simple-formatter "Project File" "-ff")
                            :transient nil
                            #'projectile-find-file)
  (transient-make-int-call! proj-finder      "F"
                            (transient-simple-formatter "Reveal in Finder" "F")
                            :transient nil
                            #'+macos/reveal-project-in-finder)
  (transient-make-int-call! proj-kill        "K"
                            (transient-simple-formatter "Kill Project Buffers" "K")
                            #'projectile-kill-buffers)
  (transient-make-int-call! proj-recent      "-fr"
                            (transient-simple-formatter "Project Recent" "-fr")
                            :transient nil
                            #'projectile-recentf)
  (transient-make-int-call! proj-regexp      "r"
                            (transient-simple-formatter "Replace Regexp in Project" "r")
                            :transient nil
                            #'zimmerframe-replace-regexp)
  (transient-make-int-call! proj-related     "R"
                            (transient-simple-formatter "Project Related" "R")
                            :transient nil
                            #'+jg-projects-find-related)
  (transient-make-int-call! proj-replace     "s"
                            (transient-simple-formatter "Replace String in Project" "s")
                            :transient nil
                            #'projectile-replace)
  (transient-make-int-call! proj-run         "r"
                            (transient-simple-formatter "Run Project" "r")
                            #'projectile-run-project)
  (transient-make-int-call! proj-save        "S"
                            (transient-simple-formatter "Save Project Buffers" "S")
                            #'projectile-save-project-buffers)
  (transient-make-int-call! proj-scratch     "x"
                            (transient-simple-formatter "Project Scratch" "x")
                            :transient nil
                            #'+jg-ui-open-scratch-buffer)
  (transient-make-int-call! proj-shell       "'"
                            (transient-simple-formatter "Project Shell" "'")
                            :transient nil
                            #'projectile-run-shell)
  (transient-make-int-call! proj-switch      "p"
                            (transient-simple-formatter "Project Switch" "p")
                            :transient nil
                            #'projectile-switch-project)
  (transient-make-int-call! proj-symbol      "."
                            (transient-simple-formatter "Project Symbol" ".")
                            :transient nil
                            #'+default/search-project-for-symbol-at-point)
  (transient-make-int-call! proj-test        "t"
                            (transient-simple-formatter "Test Project" "t")
                            #'projectile-test-project)
  (transient-make-int-call! proj-browse      ">"
                            (transient-simple-formatter "Browse Projects" ">")
                            #'doom/browse-in-other-project)
  )

(defun jg-workspace--build-project-transient-groups ()
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
  )

;;;###autoload
(defun jg-workspace-build-project-transient ()
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
