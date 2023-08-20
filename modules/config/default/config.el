;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+spec-defs")
(load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(add-hook 'doom-first-file-hook
          #'(lambda ()
              (advice-add 'display-warning :before-until #'+jg-default-display-warning-ad)
              ))

(use-package! spec-handling
  :commands (run-spec-handlers spec-handling-new! spec-handling-add! spec-handling-setq)
  )

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) d\\'" prefix-re))
                  nil . "doom")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) r\\'" prefix-re))
                  nil . "reload")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) b\\'" prefix-re))
                  nil . "bindings")
                which-key-replacement-alist)))

(use-package! autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in           . doom-auto-revert-buffers-h)
  :hook (after-save         . doom-auto-revert-buffers-h)
  :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  :hook (doom-switch-window . doom-auto-revert-buffer-h)
  ;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, abuse the
  ;; heck out of file watchers _or_ aggressively poll your buffer list every X
  ;; seconds.
  ;;
  ;; Doom does this lazily instead. i.e. All visible buffers are reverted
  ;; immediately when a) a file is saved or b) Emacs is refocused (after using
  ;; another app). Meanwhile, buried buffers are reverted only when they are
  ;; switched to. This way, Emacs only ever has to operate on, at minimum, a
  ;; single buffer and, at maximum, ~10 buffers (after all, when do you ever
  ;; have more than 10 windows in any single frame?).

  )

(use-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally easymenu tree-widget timer
  :hook (doom-first-file . recentf-mode)
  :commands recentf-open-files
  :custom (recentf-save-file (concat doom-cache-dir "recentf"))
  :config
  ;; REVIEW: Use this in lieu of `doom--recentf-file-truename-fn' when we drop
  ;;   28 support. See emacs-mirror/emacs@32906819addd.
  ;; (setq recentf-show-abbreviated t)

  ;; Anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)

  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (add-hook 'doom-switch-window-hook #'doom--recentf-touch-buffer-h)
  (add-hook 'write-file-functions    #'doom--recentf-touch-buffer-h)
  (add-hook 'dired-mode-hook         #'doom--recentf-add-dired-directory-h)

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; Otherwise `load-file' calls in `recentf-load-list' pollute *Messages*
  (advice-add #'recentf-load-list :around #'doom-shut-up-a)
  )

(use-package! savehist
  ;; persist variables across sessions
  :defer-incrementally custom
  :hook (doom-first-input . savehist-mode)
  :custom (savehist-file (concat doom-cache-dir "savehist"))
  :config

  (add-hook 'savehist-save-hook #'doom-savehist-unpropertize-variables-h)
  (add-hook 'savehist-save-hook #'doom-savehist-remove-unprintable-registers-h)
  )

(use-package! saveplace
  ;; persistent point location in buffers
  :hook (doom-first-file . save-place-mode)
  :custom (save-place-file (concat doom-cache-dir "saveplace"))
)
