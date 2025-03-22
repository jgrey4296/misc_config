;;; completion/ivy/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+defs")
(local-load! "+spec-defs")
(local-load! "+extra-config")

(defer-load! (jg-evil-bindings jg-bindings-total) "+bindings")

(defalias '+ivy--switch-buffer-preview-all #'counsel--switch-buffer-update-fn)
(defalias '+ivy--switch-buffer-unwind      #'counsel--switch-buffer-unwind)

;;-- ivy

(use-package! ivy
  :hook (doom-first-input . ivy-mode)
  :init
  (let ((standard-search-fn #'ivy--regex-plus)
        (alt-search-fn      #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist `((counsel-rg     . ,standard-search-fn)
                                  (swiper         . ,standard-search-fn)
                                  (swiper-isearch . ,standard-search-fn)
                                  (t . ,alt-search-fn))
          ivy-more-chars-alist '((counsel-rg . 3)
                                 (counsel-search . 3)
                                 (t . 3))
          )
    )

  (define-key!
    [remap switch-to-buffer]              #'+ivy/switch-buffer
    [remap switch-to-buffer-other-window] #'+ivy/switch-buffer-other-window
    [remap persp-switch-to-buffer]        #'+ivy/switch-workspace-buffer
    [remap evil-show-jumps]               #'+ivy/jump-list)

  ;; Fix #4886: otherwise our remaps are overwritten
  (setq ivy-mode-map (make-sparse-keymap))

  :config
  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.

  ;; Highlight each ivy candidate including the following newline, so that it
  ;; extends to the right edge of the window
  (setf (alist-get 't ivy-format-functions-alist) #'+ivy-format-function-line-or-arrow)

  ;; Integrate `ivy' with `better-jumper'; ensure a jump point is registered
  ;; before jumping to new locations with ivy
  (setf (alist-get 't ivy-hooks-alist) (lambda ()
                                         (with-ivy-window
                                           (setq +ivy--origin (point-marker)))))

  (add-hook! 'minibuffer-exit-hook #'+ivy--set-jump-point-maybe-h)

  (after! yasnippet
    (add-hook 'yas-prompt-functions #'+ivy-yas-prompt-fn))

  (after! ivy-hydra
    ;; Ensure `ivy-dispatching-done' and `hydra-ivy/body' hydras can be
    ;; exited / toggled by the same key binding they were opened
    (add-to-list 'ivy-dispatching-done-hydra-exit-keys '("C-o" nil))
    (defhydra+ hydra-ivy () ("M-o" nil)))

  )

(use-package! ivy-rich
  :after ivy
  :config
  ;; Enahnce the appearance of a couple counsel commands
  (plist-put! ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                 (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
              'counsel-M-x
              '(:columns
                ((counsel-M-x-transformer (:width 60))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
              'counsel-projectile-switch-to-buffer
              (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
              'counsel-bookmark
              '(:columns
                ((ivy-rich-candidate (:width 0.5))
                 (ivy-rich-bookmark-filename-or-empty-p (:width 60)))))

  (ivy-set-display-transformer 'internal-complete-buffer nil)

  ;; Highlight buffers differently based on whether they're in the same project
  ;; as the current project or not.
  (when-let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
              (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (setcar switch-buffer-alist '+ivy-rich-buffer-name))

  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1)
  )

(use-package! ivy-avy :after ivy)

(use-package! ivy-prescient
  :when (modulep! +prescient)
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :config
  ;; REVIEW Remove when radian-software/prescient.el#102 is resolved
  (add-to-list 'ivy-sort-functions-alist '(ivy-resume))

  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer lsp-ivy-workspace-symbol
          ivy-resume ivy--restore-session counsel-grep counsel-git-grep
          counsel-rg counsel-ag counsel-ack counsel-fzf counsel-pt counsel-imenu
          counsel-yank-pop counsel-recentf counsel-buffer-or-recentf
          counsel-outline counsel-org-goto counsel-jq)
        ivy-prescient-retain-classic-highlighting t)

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el"))
  )

(use-package! ivy-hydra
  :defer t
  )

;;-- end ivy

;;-- counsel

(use-package! counsel
  :after ivy
  :init
  :config
  (when (stringp counsel-rg-base-command)
    ;; REVIEW Counsel allows `counsel-rg-base-command' to be a string or list.
    ;;        This backwards compatibility complicates things for Doom. Simpler to
    ;;        just force it to always be a list.
    (setq counsel-rg-base-command (split-string counsel-rg-base-command)))

  ;; Make `counsel-compile' projectile-aware (if you prefer it over
  ;; `+ivy/compile' and `+ivy/project-compile')
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)

  (after! savehist
    ;; Persist `counsel-compile' history
    (add-to-list 'savehist-additional-variables 'counsel-compile-history))

  ;; `counsel-imenu' -- no sorting for imenu. Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))

  ;; `counsel-locate'
  (when IS-MAC
    ;; Use spotlight on mac by default since it doesn't need any additional setup
    (setq counsel-locate-cmd #'counsel-locate-cmd-mdfind))

  (add-to-list 'swiper-font-lock-exclude #'+doom-dashboard-mode) ;; Don't mess with font-locking on the dashboard; it causes breakages

  ;; `counsel-find-file'
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; `counsel-search': use normal page for displaying results, so that we see
  ;; custom ddg themes (if one is set).
  (setf (nth 1 (alist-get 'ddg counsel-search-engines-alist)) "https://duckduckgo.com/?q=")

  ;;-- ivy
  (dolist (fn '(counsel-rg counsel-find-file))
    (ivy-add-actions
     fn '(("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory))))
           "insert relative path")
          ("P" (lambda (path) (with-ivy-window (insert path)))
           "insert absolute path")
          ("l" (lambda (path) (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory)))))
           "insert relative org-link")
          ("L" (lambda (path) (with-ivy-window (insert (format "[[%s]]" path))))
           "Insert absolute org-link"))))
  ;; Decorate `doom/help-custom-variable' results the same way as
  ;; `counsel-describe-variable' (adds value and docstring columns).
  (ivy-configure 'doom/help-custom-variable :parent 'counsel-describe-variable)

  ;;-- end ivy

  ;;-- hooks
  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;;-- end hooks

  ;;-- advice

  (advice-add 'counsel-projectile-find-file-action :around #'+ivy--run-from-ivy-directory-a)
  (advice-add 'counsel--find-return-list :override #'+ivy--counsel-file-jump-use-fd-rg-a)
  ;;-- end advice

  )

(use-package! counsel-projectile
  :defer t
  :init
  :config
  ;; A more sensible `counsel-projectile-find-file' that reverts to
  ;; `counsel-find-file' if invoked from $HOME, `counsel-file-jump' if invoked
  ;; from a non-project, `projectile-find-file' if in a big project (more than
  ;; `ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.
  (setf (alist-get 'projectile-find-file counsel-projectile-key-bindings)
        #'+ivy/projectile-find-file)

  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)

  (when (modulep! +prescient) (setq counsel-projectile-sort-files t))
  )

;;-- end counsel

;;-- swiper

(use-package! swiper
  :config
  (setq swiper-action-recenter t)
  )
;;-- end swiper
