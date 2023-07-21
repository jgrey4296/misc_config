;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(load! "+vars")

(defer-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")
(defer-load! jg-evil-ex-bindings "+evil-ex")


(use-package! persp-mode
  :unless noninteractive
  :commands persp-switch-to-buffer
  :hook (doom-init-ui . persp-mode)
  :config
  ;; Per-workspace `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  ;;-- hooks
  (add-hook! '(persp-mode-hook persp-after-load-state-functions)
             #'+workspaces-ensure-no-nil-workspaces-h
             )

  (add-hook! 'persp-mode-hook
             #'+workspaces-init-first-workspace-h
             #'+workspaces-init-persp-mode-h
             )

  (add-hook! 'persp-before-deactivate-functions
             #'+workspaces-save-winner-data-h
             )

  (add-hook! 'persp-activated-functions
             #'+workspaces-load-winner-data-h
    )

  ;; Fix #1973: visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)
  (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions #'doom-unreal-buffer-p)
  ;; Don't try to persist dead/remote buffers. They cause errors.
  (add-hook! 'persp-filter-save-buffers-functions
             #'+workspaces-dead-buffer-p
             #'+workspaces-remote-buffer-p
             )

  (add-hook! 'persp-after-load-state-functions
             #'+workspaces-reload-indirect-buffers-h
             )

  (after! posframe
    ;; Fix #1017: stop session persistence from restoring a broken posframe
    (add-hook! 'persp-after-load-state-functions
               #'+workspaces-delete-all-posframes-h
               ))

  ;;;; Registering buffers to perspectives
  (add-hook! 'doom-switch-buffer-hook
             #'+workspaces-add-current-buffer-h
    )

  (add-hook 'delete-frame-functions #'+workspaces-delete-associated-workspace-h)
  (add-hook 'server-done-hook #'+workspaces-delete-associated-workspace-h)

  ;; Otherwise, buffers opened via bookmarks aren't treated as "real" and are
  ;; excluded from the buffer list.
  (add-hook 'bookmark-after-jump-hook #'+workspaces-add-current-buffer-h)

  (add-hook! 'jg-projects-switch-hook
             #'+workspaces-set-project-action-fn
             #'+workspaces-switch-to-project-h
             )

  ;; tab-bar
  (add-hook! 'tab-bar-mode-hook
             #'+workspaces-set-up-tab-bar-integration-h
             )
  ;;-- end hooks

  ;;-- advice

  (defadvice! +workspaces--evil-alternate-buffer-a (&optional window)
    "Make `evil-alternate-buffer' ignore buffers outside the current workspace."
    :override #'evil-alternate-buffer
    (let* ((prev-buffers
            (if persp-mode
                (cl-remove-if-not #'persp-contain-buffer-p (window-prev-buffers)
                                  :key #'car)
              (window-prev-buffers)))
           (head (car prev-buffers)))
      (if (eq (car head) (window-buffer window))
          (cadr prev-buffers)
        head)))

  (defadvice! +workspaces-remove-dead-buffers-a (persp)
    " HACK Fixes #4196, #1525: selecting deleted buffer error when quitting Emacs
        or on some buffer listing ops. "
    :before #'persp-buffers-to-savelist
    (when (perspective-p persp)
      ;; HACK Can't use `persp-buffers' because of a race condition with its gv
      ;;      getter/setter not being defined in time.
      (setf (aref persp 2)
            (cl-delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

  ;; Don't bother auto-saving the session if no real buffers are open.
  (advice-add #'persp-asave-on-exit :around #'+workspaces-autosave-real-buffers-a)

  ;;-- end advice

  (when (modulep! :completion ivy)
    (after! ivy-rich
      (cl-callf plist-put ivy-rich-display-transformers-list
        '+workspace/switch-to
        '(:columns ((ivy-rich-candidate (:width 50))
                    (+workspace--ivy-rich-preview))))))

  ;;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; compile
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode default-directory compilation-directory
                compilation-environment compilation-arguments))

  ;; magit
  (persp-def-buffer-save/load
   :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
   :save-vars '(default-directory)
   :load-function (lambda (savelist &rest _)
                    (cl-destructuring-bind (buffer-name vars &rest _rest) (cdr savelist)
                      (magit-status (alist-get 'default-directory vars)))))

  ;; Restore indirect buffers
  (persp-def-buffer-save/load
   :tag-symbol 'def-indirect-buffer
   :predicate #'buffer-base-buffer
   :save-function (lambda (buf tag vars)
                    (list tag (buffer-name buf) vars
                          (buffer-name (buffer-base-buffer buf))))
   :load-function (lambda (savelist &rest _rest)
                    (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                        (cdr savelist)
                      (push (cons buf-name base-buf-name)
                            +workspaces--indirect-buffers-to-restore)
                      nil)))

  )

(use-package! carousel-minor-mode
  :hook (doom-init-ui . carousel-minor-mode)
  :config
  (after! ivy
    (ivy-add-actions '+jg-workspaces-ivy
                     '(("r" +jg-workspaces-rename "Rename")
                       ("l" +jg-workspaces-new-ring "new loop")
                       )
                     )
    )
  )

(use-package! project-zimmerframe
  :commands (project-zimmerframe-minor-mode zimmerframe-next)
  )

(use-package! neotree
  :commands (neotree-show neotree-hide neotree-toggle neotree-dir neotree-find neo-global--with-buffer neo-global--window-exists-p)
  :config
  (after! winner
    (add-to-list 'winner-boring-buffers neo-buffer-name))
  (add-hook! 'neo-enter-hook #'+neotree-fix-cursor-h)
  )

(use-package! related-files
  :autoload make-related!
  )
