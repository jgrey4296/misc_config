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
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
  :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  :hook (doom-switch-window . doom-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, abuse the
  ;; heck out of file watchers _or_ aggressively poll your buffer list every X
  ;; seconds. Too many watchers can grind Emacs to a halt if you preform
  ;; expensive or batch processes on files outside of Emacs (e.g. their mtime
  ;; changes), and polling your buffer list is terribly inefficient as your
  ;; buffer list grows into the hundreds.
  ;;
  ;; Doom does this lazily instead. i.e. All visible buffers are reverted
  ;; immediately when a) a file is saved or b) Emacs is refocused (after using
  ;; another app). Meanwhile, buried buffers are reverted only when they are
  ;; switched to. This way, Emacs only ever has to operate on, at minimum, a
  ;; single buffer and, at maximum, ~10 buffers (after all, when do you ever
  ;; have more than 10 windows in any single frame?).
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (doom-visible-buffers))
      (with-current-buffer buf
        (doom-auto-revert-buffer-h))))

  )

(use-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally easymenu tree-widget timer
  :hook (doom-first-file . recentf-mode)
  :commands recentf-open-files
  :custom (recentf-save-file (concat doom-cache-dir "recentf"))
  :config
  (setq recentf-auto-cleanup nil     ; Don't. We'll auto-cleanup on shutdown
        recentf-max-saved-items 200) ; default is 20

  (defun doom--recentf-file-truename-fn (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))

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

  (add-hook! '(doom-switch-window-hook write-file-functions)
    (defun doom--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))

  (add-hook! 'dired-mode-hook
    (defun doom--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory)))

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
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (add-hook! 'savehist-save-hook
    (defun doom-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring' to reduce savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))
    (defun doom-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist))))
  )

(use-package! saveplace
  ;; persistent point location in buffers
  :hook (doom-first-file . save-place-mode)
  :custom (save-place-file (concat doom-cache-dir "saveplace"))
  :config
  (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! doom--inhibit-saveplace-in-long-files-a (fn &rest args)
    :around #'save-place-to-alist
    (unless doom-large-file-p
      (apply fn args)))

  (defadvice! doom--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so this replace calls to `pp' with the much faster `prin1'."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)) (funcall fn))))
