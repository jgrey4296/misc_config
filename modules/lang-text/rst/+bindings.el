;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-rst-mode-map
      :desc "Forward Section"        :n "] ]" #'rst-forward-section
      :desc "Backward Section"       :n "[ [" #'rst-backward-section
      )

(map! :map jg-rst-mode-map
      :localleader
      :desc "Header Hierarchy" "h" #'rst-display-hdr-hierarchy
      (:prefix ("a" . "adjust")
               "a" #'rst-adjust
               "r" #'rst-adjust-region)
      (:prefix ("t" . "table of contents")
               "t" #'rst-toc
               "i" #'rst-toc-insert
               "u" #'rst-toc-update
               "f" #'rst-toc-follow-link)
      (:prefix ("i" . "insert")
       :desc "List" "l" #'rst-insert-list

       )
      )

(map! :map jinja2-mode-map
      "C-c ]" nil
      :localleader
      :desc "Open Tag"  "h"  #'jinja2-insert-tag
      :desc "Close Tag" "l"  #'jinja2-close-tag
      :desc "Insert Var" "v" #'jinja2-insert-var

      )


(after! rst
  (setq rst-mode-map jg-rst-mode-map

        )
  )
