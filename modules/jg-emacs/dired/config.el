;;; tools/dired/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")

(use-package-hook! dired :post-config
  (load! "+bindings")
)


;; open -ngF for files with permission errors
(use-package-hook! dired-x :post-config
  (setq dired-guess-shell-alist-user
        `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" "open")
          ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" "open")
          ("\\.\\(?:xcf\\)\\'" "open")
          ("\\.csv\\'" "open")
          ("\\.tex\\'" "open")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "open")
          ("\\.\\(?:mp3\\|flac\\)\\'" "open")
          ("\\.html?\\'" "open")
          ("\\.md\\'" "open"))
        )
  )
(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(use-package! diredfl
  :config
  (set-face-attribute 'diredfl-flag-mark-line nil :background "blueviolet")
)
