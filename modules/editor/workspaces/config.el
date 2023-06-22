;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(load! "+vars")

(defer-load! "+spec-defs")
(after! jg-bindings-total (load! "+bindings"))

(use-package! persp-mode
  :unless noninteractive
  :commands persp-switch-to-buffer
  :hook (doom-init-ui . persp-mode)
  :config
  ;; Per-workspace `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  ;;-- hooks
  ;;;; Create main workspace
  ;; The default perspective persp-mode creates is special and doesn't represent
  ;; a real persp object, so buffers can't really be assigned to it, among other
  ;; quirks, so I replace it with a "main" perspective.
  (add-hook! '(persp-mode-hook persp-after-load-state-functions)
             (defun +workspaces-ensure-no-nil-workspaces-h (&rest _)
               (when persp-mode
                 (dolist (frame (frame-list))
                   (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
                     ;; Take extra steps to ensure no frame ends up in the nil perspective
                     (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                             +workspaces-main)
                                         frame))))))

  (add-hook! 'persp-mode-hook
             (defun +workspaces-init-first-workspace-h (&rest _)
               "Ensure a main workspace exists."
               (when persp-mode
                 (let (persp-before-switch-functions)
                   ;; Try our best to hide the nil perspective.
                   (when (equal (car persp-names-cache) persp-nil-name)
                     (pop persp-names-cache))
                   ;; ...and create a *real* main workspace to fill this role.
                   (unless (or (persp-get-by-name +workspaces-main)
                               ;; Start from 2 b/c persp-mode counts the nil workspace
                               (> (hash-table-count *persp-hash*) 2))
                     (persp-add-new +workspaces-main))
                   ;; HACK Fix #319: the warnings buffer gets swallowed when creating
                   ;;      `+workspaces-main', so display it ourselves, if it exists.
                   (when-let (warnings (get-buffer "*Warnings*"))
                     (save-excursion
                       (display-buffer-in-side-window
                        warnings '((window-height . shrink-window-if-larger-than-buffer))))))))

             (defun +workspaces-init-persp-mode-h ()
               (cond (persp-mode
                      ;; `uniquify' breaks persp-mode. It renames old buffers, which causes
                      ;; errors when switching between perspective (their buffers are
                      ;; serialized by name and persp-mode expects them to have the same
                      ;; name when restored).
                      (when uniquify-buffer-name-style
                        (setq +workspace--old-uniquify-style uniquify-buffer-name-style))
                      (setq uniquify-buffer-name-style nil)
                      ;; Ensure `persp-kill-buffer-query-function' is last
                      (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
                      (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t)
                      ;; Restrict buffer list to workspace
                      (advice-add #'doom-buffer-list :override #'+workspace-buffer-list))
                     (t
                      (when +workspace--old-uniquify-style
                        (setq uniquify-buffer-name-style +workspace--old-uniquify-style))
                      (advice-remove #'doom-buffer-list #'+workspace-buffer-list)))))

  (add-hook! 'persp-before-deactivate-functions
             (defun +workspaces-save-winner-data-h (_)
               (when (and (bound-and-true-p winner-mode)
                          (get-current-persp))
                 (set-persp-parameter
                  'winner-ring (list winner-currents
                                     winner-ring-alist
                                     winner-pending-undo-ring)))))

  (add-hook! 'persp-activated-functions
    (defun +workspaces-load-winner-data-h (_)
      (when (bound-and-true-p winner-mode)
        (cl-destructuring-bind
            (currents alist pending-undo-ring)
            (or (persp-parameter 'winner-ring) (list nil nil nil))
          (setq winner-undo-frame nil
                winner-currents currents
                winner-ring-alist alist
                winner-pending-undo-ring pending-undo-ring)))))

  ;; Fix #1973: visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)
  (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions #'doom-unreal-buffer-p)
  ;; Don't try to persist dead/remote buffers. They cause errors.
  (add-hook! 'persp-filter-save-buffers-functions
             (defun +workspaces-dead-buffer-p (buf)
               ;; Fix #1525: Ignore dead buffers in PERSP's buffer list
               (not (buffer-live-p buf)))

             (defun +workspaces-remote-buffer-p (buf)
               ;; And don't save TRAMP buffers; they're super slow to restore
               (let ((dir (buffer-local-value 'default-directory buf)))
                 (ignore-errors (file-remote-p dir)))))
  (add-hook! 'persp-after-load-state-functions
    (defun +workspaces-reload-indirect-buffers-h (&rest _)
      (dolist (ibc +workspaces--indirect-buffers-to-restore)
        (cl-destructuring-bind (buffer-name . base-buffer-name) ibc
          (let ((base-buffer (get-buffer base-buffer-name)))
            (when (buffer-live-p base-buffer)
              (when (get-buffer buffer-name)
                (setq buffer-name (generate-new-buffer-name buffer-name)))
              (make-indirect-buffer base-buffer buffer-name t)))))
      (setq +workspaces--indirect-buffers-to-restore nil)))
  (after! posframe
    ;; Fix #1017: stop session persistence from restoring a broken posframe
    (add-hook! 'persp-after-load-state-functions
               (defun +workspaces-delete-all-posframes-h (&rest _)
                 (posframe-delete-all))))

  ;;;; Registering buffers to perspectives
  (add-hook! 'doom-switch-buffer-hook
    (defun +workspaces-add-current-buffer-h ()
      "Add current buffer to focused perspective."
      (or (not persp-mode)
          (persp-buffer-filtered-out-p
           (or (buffer-base-buffer (current-buffer))
               (current-buffer))
           persp-add-buffer-on-after-change-major-mode-filter-functions)
          (persp-add-buffer (current-buffer) (get-current-persp) nil nil))))

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
             (defun +workspaces-set-up-tab-bar-integration-h ()
               (add-hook 'persp-before-deactivate-functions #'+workspaces-save-tab-bar-data-h)
               (add-hook 'persp-activated-functions #'+workspaces-load-tab-bar-data-h)
               ;; Load and save configurations for tab-bar.
               (add-hook 'persp-before-save-state-to-file-functions #'+workspaces-save-tab-bar-data-to-file-h)
               (+workspaces-load-tab-bar-data-from-file-h)))
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

(use-package! window-ring-minor-mode
  :hook (doom-init-ui . window-ring-minor-mode)
  :config
  (after! ivy
    (ivy-add-actions '+jg-workspaces-ivy
                     '(("r" +jg-workspaces-rename "Rename")
                       ("l" +jg-workspaces-new-ring "new loop")
                       )
                     )
    )
  )

(use-package! project-walk
  :commands (project-walk-minor-mode project-walk-next)
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
