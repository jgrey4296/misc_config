;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(transient-make-int-call! magit-todos         "t"   "Todos"             :transient nil #'magit-todos-list)
(transient-make-call!     proj-clear-cache    "C"   "Project Clear Cache" (projectile-invalidate-cache nil))
(transient-make-int-call! proj-sidebar        "s"   "Sidebar"            :transient nil #'+jg-ui-tree/open)

;;-- carousel
(progn
  (transient-make-call! carousel-print       "p" "Print Order" (carousel-print-order))
  (transient-make-call! carousel-edit        "E" "Edit Order" :transient nil (carousel-edit-order))
  (transient-make-call! carousel-toggle-loop "l"
                        (format              "%-2s : Looping" (fmt-as-bool! (persp-parameter 'carousel-loop)))
                        (carousel-toggle-loop))
  (transient-make-call! carousel-new         "n" "New Carousel"   :transient nil (carousel-new))
  (transient-make-call! carousel-toggle      "t" "Toggle"         (carousel-toggle))
  (transient-make-call! carousel-quit        "T" "Quit"           (carousel-deconvert))
  (transient-make-call! carousel-expand      "e" "Expand Focus"   :transient nil (call-interactively #'carousel-expand-focus))
  (transient-make-call! carousel-reset       "r" "Reset"          :transient nil (carousel-reset-columns))
  (transient-make-call! carousel-clear       "K" "Clear Carousel" :transient nil (carousel-clear-ring))
  (transient-make-call! carousel-add-buffer  "a" "Add"            :transient nil (carousel-add-current-buffer))
  (transient-make-call! carousel-remove      "x" "Remove"         :transient nil (carousel-remove-buffer))
  (transient-make-call! carousel-move-left   "[" "Move Left" (carousel-move-buffer-left))
  (transient-make-call! carousel-move-right  "]" "Move Right" (carousel-move-buffer-right))
  (transient-make-call! carousel-claim       "w" "Claim Window" (carousel-claim-window))

  (transient-make-int-call! carousel-goto        "f" "Find-Buffer" :transient nil :interactive t #'carousel-goto-choice)
  )

;;;###autoload (autoload 'transient-carousel "ide/workspaces/autoload/transient.el" nil t)
(transient-define-prefix transient-carousel ()
  ""
  [:description (lambda () (format "Carousel: %-11s" (persp-parameter 'carousel)))
                [(transient-macro-call-carousel-toggle)
                 (transient-macro-call-carousel-quit)]
                [(transient-macro-call-carousel-move-left)]
                [(transient-macro-call-carousel-move-right)]
                ]
  [
    ["Carousel Control"
     (transient-macro-call-carousel-new)
     (transient-macro-call-carousel-clear)
     (transient-macro-call-carousel-reset)
     ]
    ["Buffer Control "
     (transient-macro-call-carousel-claim)
     (transient-macro-call-carousel-expand)
     (transient-macro-call-carousel-goto)
     ]
    [" "
     (transient-macro-call-carousel-add-buffer)
     (transient-macro-call-carousel-remove)
     (transient-macro-call-carousel-edit)
     (transient-macro-call-carousel-print)
     ]
    ]
  ["Settings"
   (transient-macro-call-carousel-toggle-loop)
   ]
  transient-quit!
  )

;;-- end carousel

;;-- zimmerframe
(progn
  (transient-make-mode-toggle! project-zimmerframe-minor-mode "Zimmerframe" "RET")
  (transient-make-int-call! zimmerframe-default-filters "SPC" "Apply Default Filters" #'zimmerframe-filter-defaults)
  (transient-make-int-call! zimmerframe-remaining "b" "Remaining List Buffer" :transient nil #'zimmerframe-remaining)
  (transient-make-int-call! zimmerframe-count   "r"  "Remaining Count" #'zimmerframe-num)
  (transient-make-int-call! zimmerframe-dir     "d"  "Set Target Directory" #'zimmerframe-directory-init)
  (transient-make-int-call! zimmerframe-filter  "f"  "Filter " #'zimmerframe-filter)
  (transient-make-int-call! zimmerframe-keep    "k"  "Keep"    #'zimmerframe-filter-keep)
  )

(transient-make-subgroup! transient-zimmerframe-control "z"
                          ""
                          :desc (lambda () (format "+Zimmerframe : %s" (if (fboundp 'zimmerframe-remaining-count)
                                                                           (zimmerframe-remaining-count)
                                                                         0)))
                          [
                           [
                            (transient-macro-toggle-project-zimmerframe-minor-mode)
                            (transient-macro-call-zimmerframe-default-filters)
                            ]
                           [
                            (transient-macro-call-zimmerframe-remaining)
                            (transient-macro-call-zimmerframe-count)
                            ]
                           [
                            (transient-macro-call-zimmerframe-filter)
                            (transient-macro-call-zimmerframe-keep)
                            ]
                           ]
                          )
;;-- end zimmerframe

;;-- workspace management
(progn
  (transient-make-call! proj-root            "`"   "Project Root" (find-file (projectile-project-root)))

  (transient-make-int-call! proj-add         "a"   "Add Project"               #'projectile-add-known-project)
  (transient-make-int-call! proj-clean       "c"   "Clean Project"             #'+jg-projects-clean)
  (transient-make-int-call! proj-clear-known "D"   "Clear Project List Cache"  #'projectile-clear-known-projects)
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
  (transient-make-int-call! proj-regexp      "r"   "Replace Regexp in Project"        :transient nil #'projectile-replace-regexp)
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

;;-- end workspace management

;;-- workspace
(progn
  ;; Projects
  (transient-make-call! goto-root                "`" "Goto-Root"                 :transient nil (find-file (doom-project-root)))
  (transient-make-call! zimmerframe-next         "w" "Walk Next"                 (zimmerframe-next))
  (transient-make-call! zimmerframe-prev         "W" "Walk Prev"                 (zimmerframe-prev))
  (transient-make-call! debug-project-type       "?" "Debug Project Type"        (+jg-projects-detect-type))
  (transient-make-call! recent-files             "r" "Project Recent Files"      :transient nil (projectile-recentf))

  ;; Workspaces

  ;; Windows
  (transient-make-var-toggle! auto-balance evil-auto-balance-windows "Auto-Balance Windows" "B")

  (transient-make-call! shrink-horizontally "h" "Horizontal Shrink" (window-resize transient--original-window -5 t t))
  (transient-make-call! shrink-vertically   "v" "Vertical Shrink"   (window-resize transient--original-window -5 nil t))
  (transient-make-call! grow-horizontally   "H" "Horizontal Grow"   (window-resize transient--original-window 5 t t))
  (transient-make-call! grow-vertically     "V" "Vertical Grow"     (window-resize transient--original-window 5 nil t))

  (transient-make-call! toggle-layout       "/" "Toggle Layout"     (+jg-ui-window-layout-toggle))
  (transient-make-call! rotate-layout       "\\" "Rotate Layout"    (+jg-ui-window-rotate-forward))

  (transient-make-call! toggle-dedication   "!"
                        (format "%-2s : Window Dedicated: %s"
                                (fmt-as-bool! (window-dedicated-p (selected-window)))
                                (window-buffer (selected-window)))
                        (let ((curr-window (selected-window)))
                          (set-window-dedicated-p curr-window (not (window-dedicated-p curr-window)))
                          (if (window-dedicated-p curr-window)
                              (message "Window is now dedicated to %s" (window-buffer curr-window))
                            (message "Window is un-dedicated"))
                          )
  )

  (transient-make-int-call! window-delete      "d" "Delete Window"   #'+workspace/close-window-or-workspace)

  (transient-make-int-call! window-split-below "-" "Split Below"     #'split-window-below)
  (transient-make-int-call! window-split-right "=" "Split Right"     #'split-window-right)
  (transient-make-int-call! window-maximize    "m" "Maximize Window" #'doom/window-maximize-buffer)
  (transient-make-int-call! window-undo        "u" "Window Undo"     #'winner-undo)
  (transient-make-int-call! window-redo        "U" "Window Redo"     #'winner-redo)
  (transient-make-call!     window-balance     "b" "Balance Windows" (balance-windows))
  )

;;;###autoload (autoload 'transient-workspace "ide/workspaces/autoload/transient.el" nil t)
(transient-define-prefix transient-workspace ()
  ""
  [
   ["Project Locs"
    (transient-macro-call-goto-root)
    (transient-macro-call-proj-sidebar)
    (transient-macro-call-magit-todos)
    (transient-macro-call-recent-files)
    (transient-macro-call-zimmerframe-next)
    (transient-macro-call-zimmerframe-prev)
    ]
   ["Project Settings"
    (transient-macro-call-debug-project-type)
    (transient-macro-call-proj-clear-cache)
    ]
   [" "
    transient-project-actions
    transient-project
    transient-all-projects
    transient-zimmerframe-control
    ]
   ]
  [
   ["Window Sizes"
     (transient-macro-call-shrink-horizontally)
     (transient-macro-call-shrink-vertically)
     (transient-macro-call-grow-horizontally)
     (transient-macro-call-grow-vertically)
     (transient-macro-call-window-maximize)
     (transient-macro-call-window-balance)
     ]
   ["Window Layouts"
    (transient-macro-call-toggle-layout)
    (transient-macro-call-rotate-layout)
    (transient-macro-call-window-split-below)
    (transient-macro-call-window-split-right)
    (transient-macro-call-window-delete)
    ]
   ["Winner"
    (transient-macro-call-window-undo)
    (transient-macro-call-window-redo)
    ]
   ["Window Settings"
    (transient-macro-toggle-auto-balance)
    (transient-macro-call-toggle-dedication)
    ]
   ]
  ["Workspaces"
   ["New"]
   ["Rename"]
   ["Delete"]
   ["Select"]
   ["Shell"]
   ]
  [
   ["Workspace Settings"
    ""
    ]
   ]
  transient-quit!
  )

;;-- end workspace
