;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 25, 2023
;; Modified: May 25, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! so-long
  :hook (doom-first-file . global-so-long-mode)
  :config
  (setq so-long-predicate #'doom-buffer-has-long-lines-p)

  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)

  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              ;; These are redundant on Emacs 29+
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode)
            )
)

(use-package! vlf
  :defer t
  :config
  (require 'vlf-setup)
  (setq vlf-batch-size 1000000
        vlf-application 'ask
        large-file-warning-threshold 1000000
        vlf-forbidden-modes-list '(archive-mode
                                   tar-mode
                                   jka-compr
                                   git-commit-mode
                                   image-mode
                                   doc-view-mode
                                   doc-view-mode-maybe
                                   ebrowse-tree-mode
                                   )

        )
  )

;;; config.el ends here
