;;; +bind-extra.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;-- zimmerframe
(map! :map jge-motion--op-fw-map
      :desc "Walk Next" "z" #'zimmerframe-next
      :map jge-motion--op-bw-map
      :desc "Walk Back" "z" #'zimmerframe-prev
      )

;;-- end zimmerframe

;;-- ibuffer
(map! :map jg-ibuffer-filter-map
      (:prefix ("b" . "by")
       :desc "filter-by-workspace"   "w" #'ibuffer-filter-by-workspace-buffers
       :desc "filter-by-carousel" "R" #'ibuffer-filter-by-carousel-buffers
      )
      (:prefix "g"
       :desc "Group by Workspace"   "w" (ibuffer-generate! (+jg-ibuffer-generate-workspace-groups))
       )
      )
;;-- end ibuffer

;;-- carousel
(map! :map carousel-edit-map
      "C-c C-c" #'carousel-edit-commit)

(map! :map carousel-minor-mode-map
      :n "R" #'jg-workspace-run-carousel-transient
      )

;;-- end carousel

(map! :map jge-motion--op-bw-map
      :desc "ring window"  "r"    #'carousel-move-focus-alt
      :desc "ring oldest"  "R"    #'carousel-goto-oldest
      :desc "workspace"    "w"    #'+workspace/switch-left
      )

(map! :map jge-motion--op-fw-map
      :desc "ring window"  "r"    #'carousel-move-focus
      :desc "ring newest"  "R"    #'carousel-goto-newest
      :desc "workspace"    "w"    #'+workspace/switch-right
)

(map! :map jg-dired-mode-map
      :after jg-dired-bindings
      (:prefix "["
       :desc "workspace"    "w"    #'+workspace/switch-left
       )
      (:prefix "]"
       :desc "workspace"    "w"    #'+workspace/switch-right
       )
)

;;; +bind-extra.el ends here
