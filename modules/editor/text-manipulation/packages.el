;; -*- no-byte-compile: t; -*-
;;; util/text/packages.el
(package! academic-phrases)
(package! evil-string-inflection)
(package! lorem-ipsum)
(package! rainbow-mode)
(package! highlight-parentheses)
(package! helm-wordnet)
(package! wordnut)
(package! rotate-text)
(package! license-templates)
(package! vundo)
(package! undo-fu)
(package! lint-result-mode :recipe `(:local-repo ,(expand-file-name "packages/major-modes/lint-result-mode" doom-user-dir)))