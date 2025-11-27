;;; treesit-minor-mode.el -*- lexical-binding: t; no-byte-compile: t;-*-
;;-- header
;; File Commentary:
;;
;;
;;-- end header

;;-- imports

;;-- end imports

;;-- vars

;;-- end vars

;;-- mode definition

(defvar treesit-minor--explorer-buffer "*treesit explorer*")

(defun treesit-minor-enter-explorer ()
  (interactive)
  (let ((lang (treesit-parser-language treesit-primary-parser)))

    (setq-local treesit--explorer-buffer (get-buffer-create treesit-minor--explorer-buffer)
                treesit--explorer-language lang
                treesit--explorer-last-node nil
                )
    (with-current-buffer treesit--explorer-buffer (treesit--explorer-tree-mode))
    (treesit--explorer-refresh)
    (treesit-explore-mode)
    )
  )

;;;###autoload
(define-minor-mode treesit-minor-mode
    " minor mode for binding treesit commands "
    :init-value nil
    :lighter "Treesit"
    ;; :global t
    :keymap (make-sparse-keymap)

)

;;;###autoload
(defun treesit-minor-mode/turn-on ()
 (unless (minibufferp)
    (if (eq major-mode 'prog-mode)
        (treesit-minor-mode 1))
    )
)

(define-globalized-minor-mode global-treesit-minor-mode treesit-minor-mode #'treesit-minor-mode/turn-on)

;;-- end mode definition

(provide 'treesit-minor-mode)

;;-- footer
;; Copyright (C) 2025 john
;;
;; Author: john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created: October 05, 2025
;; Modified: October 05, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; treesit-minor-mode.el ends here
