;;; tools/dired/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")

(use-package-hook! dired :post-config
  (load! "+bindings")
)

(use-package-hook! dired-x :post-config
  (setq dired-guess-shell-alist-user
        `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" "open -ngF")
          ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" "open -ngF")
          ("\\.\\(?:xcf\\)\\'" "open -ngF")
          ("\\.csv\\'" "open -ngF")
          ("\\.tex\\'" "open -ngF")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "open -ngF")
          ("\\.\\(?:mp3\\|flac\\)\\'" "open -ngF")
          ("\\.html?\\'" "open")
          ("\\.md\\'" "open -ngF"))
        )
  )
(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(use-package! diredfl
  :config
  (set-face-attribute 'diredfl-flag-mark-line nil :background "blueviolet")
)
