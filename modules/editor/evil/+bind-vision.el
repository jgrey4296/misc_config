;;; +evil-vision-bindings.el -*- lexical-binding: t; -*-

(map! :map jgb-vision--root-map
      ;; RET, 1, aAdocrjkIi
      :desc "Visual Mark Mode"    "0" #'evil-visual-mark-mode
      :desc "Visual Mark Mode"    "-" #'+jg-evil-delete-markers
      :desc "Widen"         "DEL" #'widen
      :desc "Widen"         "w"   #'widen
      :desc "Scroll Right"  "l"   #'evil-scroll-column-right
      :desc "Scroll Left"   "h"   #'evil-scroll-column-left

      :desc "Center" "z"          #'evil-scroll-line-to-center
      :desc "Top"    "t"          #'+jg-evil-offset-line-to-top
      :desc "Bottom" "b"          #'evil-scroll-line-to-bottom

      (:prefix ("'" . "Highlight")) ;; Reserved
      (:prefix ("v" . "Vimish Fold"))
      )
