;;; +bindings.el -*- lexical-binding: t; -*-

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


(after! rst
  (setq rst-mode-map jg-rst-mode-map

        )
  )
