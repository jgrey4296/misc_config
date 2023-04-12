;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-dired-mode-map
      :prefix ("> p" . "Pdf calls")
      :desc "Pdf Info"    "i" #'pdf-meta-extract-info
      :desc "Pdf Split"   "s" #'pdf-meta-split
      :desc "Pdf Join"    "j" #'pdf-meta-join
      :desc "Pdf Attach"  "a" #'pdf-meta-attach
      :desc "Pdf Unpack"  "u" #'pdf-meta-detach
      )

(map! :map pdf-meta-mode-map
      "!" #'pdf-meta-update-info
      )
