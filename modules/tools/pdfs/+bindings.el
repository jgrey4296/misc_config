;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-dired-mode-map
      :prefix ("c ;" . "File Type Specific")
      :desc "Pdf Info"   :n "i" #'pdf-meta-extract-info
      :desc "Pdf Split"  :n "s" #'pdf-meta-split
      :desc "Pdf Join"   :n "j" #'pdf-meta-join
      :desc "Pdf Attach" :n "a" #'pdf-meta-attach
      :desc "Pdf Unpack" :n "u" #'pdf-meta-detach
      )

(map! :map pdf-meta-mode-map
      "!" #'pdf-meta-update-info
      )
