;; -*- no-byte-compile: t; -*-
;;; util/text/packages.el
(package! adaptive-wrap)
(package! exec-path-from-shell)
(package! flycheck)
(package! fsm)
(package! lint-result-mode :recipe `(:local-repo ,(expand-file-name "packages/major-modes/lint-result-mode" doom-user-dir)))
(package! origami)
(package! rotate-text)
(package! shell-pop)
(package! timeline-mode :recipe `(:local-repo ,(expand-file-name "packages/major-modes/timeline-mode" doom-user-dir)))
(package! writegood-mode)

;; (package! objed)
