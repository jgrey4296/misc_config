;;; completion/ivy/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(use-package! swiper
  :config
  (setq swiper-action-recenter t)
  )

;;-- ivy

(use-package! ivy
  :hook (doom-first-input . ivy-mode)
  :after counsel
  :init
  (let ((standard-search-fn (if (modulep! +prescient)
                                #'+ivy-prescient-non-fuzzy
                              #'ivy--regex-plus))
        (alt-search-fn (if (modulep! +fuzzy)
                           #'ivy--regex-fuzzy
                         ;; Ignore order for non-fuzzy searches by default
                         #'ivy--regex-ignore-order)))
    (setq ivy-re-builders-alist `((counsel-rg     . ,standard-search-fn)
                                  (swiper         . ,standard-search-fn)
                                  (swiper-isearch . ,standard-search-fn)
                                  (t . ,alt-search-fn))
          ivy-more-chars-alist '((counsel-rg . 1)
                                 (counsel-search . 2)
                                 (t . 3)))
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

  ;; Override default insert action
  (ivy-set-actions t `(("i" +jg-ivy--action-insert "insert")))

  )

(use-package! ivy-rich
  :after ivy
  :config
  (defun ivy-rich-bookmark-filename-or-empty (candidate)
    (let ((filename (ivy-rich-bookmark-filename candidate)))
      (if (not filename) "" filename)))

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
                 (ivy-rich-bookmark-filename-or-empty (:width 60)))))

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

(use-package! ivy-posframe
  :when (modulep! +childframe)
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 10
        ivy-posframe-parameters `((min-width . 90) (min-height . ,ivy-height)))

  ;; default to posframe display function
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'+ivy-display-at-frame-center-near-bottom-fn)

  ;; posframe doesn't work well with async sources (the posframe will
  ;; occasionally stop responding/redrawing), and causes violent resizing of the
  ;; posframe.
  (dolist (fn '(swiper counsel-rg counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback))

  (add-hook 'doom-after-reload-hook #'posframe-delete-all)
  )

;;-- end ivy

;;-- counsel

(use-package! counsel
  :defer t
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

  (ivy-add-actions
   'counsel-rg ; also applies to `counsel-rg'
   '(("O" +ivy-git-grep-other-window-action "open in other window")))
  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file))

;;-- end ivy

;;-- hooks
  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (add-hook 'counsel-grep-post-action-hook #'recenter)

;;-- end hooks

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

(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t)
  )

(use-package! flx
  :when (modulep! +fuzzy)
  :after ivy
  :preface
  (when (or (not (modulep! +fuzzy))
            (modulep! +prescient))
    (setq ivy--flx-featurep nil))
  :init
  (setq ivy-flx-limit 10000)
  )

(use-package! amx
  :config
  ; used by `counsel-M-x'
  (setq amx-save-file (concat doom-cache-dir "amx-items"))
  )

(use-package! general-insert :defer t)
