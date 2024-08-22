;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-dired-mode-map
      :after jg-dired-bindings
      :prefix ("> p" . "Pdf calls")
      :desc "Pdf Info"        "i" #'pdf-meta-extract-info
      :desc "Pdf Split"       "s" #'pdf-meta-split
      :desc "Pdf Join"        "j" #'pdf-meta-join
      :desc "Pdf Attach"      "a" #'pdf-meta-attach
      :desc "Pdf Unpack"      "u" #'pdf-meta-detach
      :desc "Pdf ToText"      "t" #'+jg-pdf-to-text
      :desc "Pdf Linearize"   "l" #'+jg-pdf-linearize
      :desc "Pdf Validate"    "v" #'+jg-pdf-validate
      :desc "Pdf metadata"    "m" #'+jg-pdf-metadata
      :desc "Pdf decrypt"     "d" #'+jg-pdf-decrypt
      )

(map! :map pdf-meta-mode-map
      "!" #'pdf-meta-update-info
      )
