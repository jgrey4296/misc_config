;;; config/default/config.el -*- lexical-binding: t; -*-


(advice-add 'newline-and-indent        :before-until #'+default--newline-indent-and-continue-comments-a)
(advice-add 'save-place-find-file-hook :after-while #'doom--recenter-on-load-saveplace-a)
(advice-add 'save-place-to-alist       :around #'doom--inhibit-saveplace-in-long-files-a)
(advice-add 'save-place-alist-to-file  :around #'doom--dont-prettify-saveplace-cache-a)
(advice-add 'delete-backward-char      :override #'+default--delete-backward-char-a)
(advice-add 'file-notify-rm-watch      :before #'file-notify-rm-watch-silent-advice)
(advice-add 'display-warning           :before-until #'+jg-default-display-warning-ad)

(defer-load! jg-bindings-total "+bindings")

;; (add-hook 'tty-setup-hook nil)
(add-hook 'tty-setup-hook #'evil-terminal-cursor-changer-activate)
(add-hook 'tty-setup-hook #'doom-init-clipboard-in-tty-emacs-h)

(use-package! spec-handling
  ;; :autoload (speckler-add! speckler-new! speckler-setq! speckler-new-hook!)
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

(use-package! cl-lib)

(use-package! a)

(use-package! f)

(use-package! s)

(use-package! macro-tools
  :autoload upfun!
  )

(local-load! "+spec-defs")
(local-load! "+vars")
