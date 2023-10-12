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

(use-package! epa
  :init
  (setq epa-armor t
        epa-textmode t
        epg-pinentry-mode 'loopback ;; Force gpg-agent to use minibuffer
        )

  (defun +default--dont-prompt-for-keys-a (&rest _)
   " And suppress prompts if epa-file-encrypt-to has a default value (without
    overwriting file-local values). "
    (unless (local-variable-p 'epa-file-encrypt-to)
      (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to))))

  (advice-add #'epa-file-write-region :before #'+default--dont-prompt-for-keys-a)
  )

(use-package! epa-hook
  :init
  (setq-default epa-file-name-regexp "\\.\\(gpg\\|asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'"
                epa-file-encrypt-to (unless (string-empty-p user-full-name)
                                          (when-let (context (ignore-errors (epg-make-context)))
                                            (cl-loop for key in (epg-list-keys context user-full-name 'public)
                                                     for subkey = (car (epg-key-sub-key-list key))
                                                     if (not (memq 'disabled (epg-sub-key-capability subkey)))
                                                     if (< (or (epg-sub-key-expiration-time subkey) 0)
                                                           (time-to-seconds))
                                                     collect (epg-sub-key-fingerprint subkey)))
                                        )
                )
  :config
  (epa-file-name-regexp-update)
  )
