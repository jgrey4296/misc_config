;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(transient-make-call!   quickscope (format ""))

;;-- workspace
(progn
  ;; Projects
  (transient-make-call! goto-root            "`" "Goto-Root"                 :transient nil (find-file (doom-project-root)))
  (transient-make-call! zimmerframe-next     "w" "Walk Next"                 (zimmerframe-next))
  (transient-make-call! neotree-this-file    "s" "Open Sidebar"              :transient nil (+neotree/find-this-file))
  (transient-make-call! magit-todos          "t" "Project Todos"             :transient nil (magit-todos-list))
  (transient-make-call! debug-project-type   "?" "Debug Project Type"        (+jg-projects-detect-type))
  (transient-make-call! invalidate-cache     "C" "Invalidate Project Cache"  :transient nil (projectile-invalidate-cache nil))
  (transient-make-call! current-editorconfig "e" "Find current editorconfig" :transient nil (editorconfig-find-current-editorconfig))
  (transient-make-call! dir-locals           "l" "Edit Dir-Locals"           :transient nil (projectile-edit-dir-locals))
  (transient-make-call! open-configs         "c" "Project Config File"       :transient nil (+jg-projects-open-configs))
  (transient-make-call! recent-files         "r" "Project Recent Files"      :transient nil (projectile-recentf))

  ;; Workspaces

  ;; Windows
  (transient-make-var-toggle! auto-balance evil-auto-balance-windows "Auto-Balance Windows" "b")

  (transient-make-call! shrink-horizontally                          "h" "Horizontal Window Shrink" (shrink-window-horizontally))
  (transient-make-call! shrink-vertically                            "v" "Vertical Window Shrink"   (shrink-window))

  (transient-make-call! toggle-layout                                "/" "Toggle Layout"            (+jg-ui-window-layout-toggle))
  (transient-make-call! rotate-layout                                "\\" "Rotate Layout"           (+jg-ui-window-rotate-forward))
  (transient-make-call! toggle-dedication                            "d"
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

  )

;;;###autoload (autoload 'transient-workspace "ide/workspaces/autoload/transient.el" nil t)
(transient-define-prefix transient-workspace ()
  ""
  [
   ["Project Locs"
    (transient-macro-call-goto-root)
    (transient-macro-call-neotree-this-file)
    (transient-macro-call-magit-todos)
    (transient-macro-call-recent-files)
    ]
   ["Project Files"
    (transient-macro-call-current-editorconfig)
    (transient-macro-call-dir-locals)
    (transient-macro-call-open-configs)
    ]
   ["Project Walking"
    (transient-macro-call-zimmerframe-next)
    ]
   ]
  [
   ["Window Sizes"
     (transient-macro-call-shrink-horizontally)
     (transient-macro-call-shrink-vertically)
    "Maximize"
     ]
   ["Window Layouts"
    (transient-macro-call-toggle-layout)
    (transient-macro-call-rotate-layout)
    "Split Below"
    "Split Right"
    "Delete"
    ]
   ["Winner"
    "Undo"
    "Redo"
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
   ["Project Settings"
    (transient-macro-call-debug-project-type)
    (transient-macro-call-invalidate-cache)
    ]
   ["Window Settings"
    (transient-macro-toggle-auto-balance)
    (transient-macro-call-toggle-dedication)
    ]
   ["Workspace Settings"
    ""
    ]
   ]
  transient-quit!
  )

;;-- end workspace

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
     (transient-macro-call-carousel-claim-window)
     (transient-macro-call-carousel-expand)
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
