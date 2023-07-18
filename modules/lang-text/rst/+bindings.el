;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map rst-mode-map
      :localleader
      (:prefix ("a" . "adjust")
               "a" #'rst-adjust
               "r" #'rst-adjust-region)
      (:prefix ("t" . "table of contents")
               "t" #'rst-toc
               "i" #'rst-toc-insert
               "u" #'rst-toc-update
               "f" #'rst-toc-follow-link)
      )
