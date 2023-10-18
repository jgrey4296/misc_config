;;; +evil-vision-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-vision-map
      :desc "Visual Mark Mode"    "0" #'evil-visual-mark-mode
      :desc "Visual Mark Mode"    "-" #'evil-visual-mark-mode
      ;; RET, 1, aAdocrjkIi
      :desc "Widen"         "DEL" #'widen
      :desc "Widen"         "w"   #'widen
      :desc "Scroll Right"  "l"   #'evil-scroll-column-right
      :desc "Scroll Left"   "h"   #'evil-scroll-column-left

      :desc "Center" "z"          #'evil-scroll-line-to-center
      :desc "Top"    "t"          #'evil-scroll-line-to-top
      :desc "Bottom" "b"          #'evil-scroll-line-to-bottom

      :desc "Update Syntax Highlighting" "u" #'font-lock-update
      (:prefix ("'" . "Highlight")
       ;; Reserved
       )

      (:prefix ("v" . "Vimish Fold")
       ;; Reserved
       )
      )
