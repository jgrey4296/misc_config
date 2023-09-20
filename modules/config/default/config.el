;;; config/default/config.el -*- lexical-binding: t; -*-

(local-load! "+spec-defs")
(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

;; (add-hook 'tty-setup-hook nil)
(add-hook 'tty-setup-hook #'evil-terminal-cursor-changer-activate)
(add-hook 'tty-setup-hook #'doom-init-clipboard-in-tty-emacs-h)

(use-package! spec-handling
  :commands (run-spec-handlers spec-handling-new! spec-handling-add! spec-handling-setq)
  )

(use-package! osx-trash
  :commands osx-trash-move-file-to-trash
  :init
  ;; Delete files to trash on macOS, as an extra layer of precaution against
  ;; accidentally deleting wanted files.
  (setq delete-by-moving-to-trash t)

  ;; Lazy load `osx-trash'
  (when (not (fboundp 'system-move-file-to-trash))
    (defun system-move-file-to-trash (file)
      "Move FILE to trash."
      (when (and (not IS-LINUX)
                 (not (file-remote-p default-directory)))
        (osx-trash-move-file-to-trash file)))))
