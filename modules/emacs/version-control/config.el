;;; emacs/jg-vc/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(after! evil
  (load! "+conflict-merge-state")
  )
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! smerge-mode
  :after jg-bindings-total
  )

(use-package! magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package eieio transient)
  :init
  :config
  ;; The default location for git-credential-cache is in
  ;; ~/.cache/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket (doom-glob (or (getenv "XDG_CACHE_HOME")
                                                              "~/.cache/")
                                                          "git/credential/socket")))

  (add-to-list 'doom-debug-variables 'magit-refresh-verbose)

  (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    ;; Since the project likely now contains new files, best we undo the
    ;; projectile cache so it can be regenerated later.
    (projectile-invalidate-cache nil)
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))

  ;; Center the target file, because it's poor UX to have it at the bottom of
  ;; the window after invoking `magit-status-here'.
  (advice-add #'magit-status-here :after #'doom-recenter-a)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.
  (add-hook! 'magit-status-mode-hook
    (defun +magit-optimize-process-calls-h ()
      (when-let (path (executable-find magit-git-executable t))
        (setq-local magit-git-executable path))))

  (add-hook! 'magit-diff-visit-file-hook
    (defun +magit-reveal-point-if-invisible-h ()
      "Reveal the point if in an invisible region."
      (if (derived-mode-p 'org-mode)
          (org-reveal '(4))
        (require 'reveal)
        (reveal-post-command))))

  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (add-hook! 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))

  (add-hook! 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))

  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; so magit buffers can be switched to (except for process buffers)
  (add-hook! 'doom-real-buffer-functions
    (defun +magit-buffer-p (buf)
      (with-current-buffer buf
        (and (derived-mode-p 'magit-mode)
             (not (eq major-mode 'magit-process-mode))))))

  (add-hook! 'magit-section-mode-hook
    (add-hook! 'window-configuration-change-hook :local
      (defun +magit-enlargen-fringe-h ()
        "Make fringe larger in magit."
        (and (display-graphic-p)
             (derived-mode-p 'magit-section-mode)
             +magit-fringe-size
             (let ((left  (or (car-safe +magit-fringe-size) +magit-fringe-size))
                   (right (or (cdr-safe +magit-fringe-size) +magit-fringe-size)))
               (set-window-fringes nil left right))))))

  )

(use-package! forge
  :when (modulep! +forge)
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-database-file (concat doom-data-dir "forge/forge-database.sqlite"))
  (setq forge-add-default-bindings (not (modulep! :editor evil +everywhere)))
  :config
  (defadvice! +magit--forge-get-repository-lazily-a (&rest _)
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."
    :before-while #'forge-get-repository
    (file-executable-p emacsql-sqlite-executable))

  (defadvice! +magit--forge-build-binary-lazily-a (&rest _)
    "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
    :before #'forge-dispatch
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2)
      (if (not (file-executable-p emacsql-sqlite-executable))
          (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                           "See *Compile-Log* buffer for details"))
        ;; HACK Due to changes upstream, forge doesn't initialize completely if
        ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
        ;;      manually after installing it.
        (setq forge--sqlite-available-p t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
        (after! forge-topic
          (dolist (hook forge-bug-reference-hooks)
            (add-hook hook #'forge-bug-reference-setup))))))

  )

(use-package! code-review
  :when (modulep! +forge)
  :after magit
  :init
  ;; TODO This needs to either a) be cleaned up or better b) better map things
  ;; to fit
  (after! evil-collection-magit
    (dolist (binding evil-collection-magit-mode-map-bindings)
      (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
        (dolist (state states)
          (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
    )
  (spec-handling-add! evil-initial
                      '(code-review-mode evil-default-state))
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-code-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review))))

(use-package! magit-todos
  :after magit
  )

(use-package! evil-collection-magit
  :when (modulep! :editor evil +everywhere)
  :defer t
  )

(use-package! evil-collection-magit-section
  :when (modulep! :editor evil +everywhere)
  :defer t
)


;;; Default styles

(when (modulep! +pretty)
  ;; UI: make the fringe small enough that the diff bars aren't too domineering,
  ;;   while leaving enough room for other indicators.
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  ;; UI: the gutter looks less cramped with some space between it and  buffer.
  (setq-default fringes-outside-margins t)

  ;; STYLE: Redefine fringe bitmaps to take up only half the horizontal space in
  ;;   the fringe. This way we avoid overbearingly large diff bars without
  ;;   having to shrink the fringe and sacrifice precious space for other fringe
  ;;   indicators (like flycheck or flyspell).
  ;; REVIEW: Extract these into a package with faces that themes can target.
  (if (not (modulep! +diff-hl))
      (after! git-gutter-fringe
        (define-fringe-bitmap 'git-gutter-fr:added [224]
          nil nil '(center repeated))
        (define-fringe-bitmap 'git-gutter-fr:modified [224]
          nil nil '(center repeated))
        (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
          nil nil 'bottom))
    (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest args)
      :override #'diff-hl-define-bitmaps
      (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
      (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
    (defun +vc-gutter-type-face-fn (type _pos)
      (intern (format "diff-hl-%s" type)))
    (defun +vc-gutter-type-at-pos-fn (type _pos)
      (if (eq type 'delete)
          'diff-hl-bmp-delete
        'diff-hl-bmp-middle))
    (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'+vc-gutter-type-at-pos-fn)
    (advice-add #'diff-hl-fringe-bmp-from-type :override #'+vc-gutter-type-at-pos-fn)
    (setq diff-hl-draw-borders nil)
    (add-hook! 'diff-hl-mode-hook
      (defun +vc-gutter-fix-diff-hl-faces-h ()
        (mapc (doom-rpartial #'set-face-background nil)
              '(diff-hl-insert
                diff-hl-delete
                diff-hl-change)))))

  ;; FIX: To minimize overlap between flycheck indicators and git-gutter/diff-hl
  ;;   indicators in the left fringe.
  (after! flycheck
    ;; Let diff-hl have left fringe, flycheck can have right fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))


;;
;;; git-gutter

(use-package! git-gutter
  :unless (modulep! +diff-hl)
  :commands git-gutter:revert-hunk git-gutter:stage-hunk git-gutter:previous-hunk git-gutter:next-hunk
  :init
  (add-hook! 'find-file-hook
    (defun +vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (let ((file-name (buffer-file-name (buffer-base-buffer))))
        (cond
         ((and (file-remote-p (or file-name default-directory))
               (not +vc-gutter-in-remote-files)))
         ;; UX: If not a valid file, wait until it is written/saved to activate
         ;;   git-gutter.
         ((not (and file-name (vc-backend file-name)))
          (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
         ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
         ;;   type of frame we're in. This allows git-gutter to work for silly
         ;;   geese who open both tty and gui frames from the daemon.
         ((if (and (display-graphic-p)
                   (require 'git-gutter-fringe nil t))
              (setq-local git-gutter:init-function      #'git-gutter-fr:init
                          git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                          git-gutter:clear-function     #'git-gutter-fr:clear
                          git-gutter:window-width -1)
            (setq-local git-gutter:init-function      'nil
                        git-gutter:view-diff-function #'git-gutter:view-diff-infos
                        git-gutter:clear-function     #'git-gutter:clear-diff-infos
                        git-gutter:window-width 1))
          (unless (memq major-mode git-gutter:disabled-modes)
            (git-gutter-mode +1)
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))

  ;; UX: Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;;   syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;;   global minor modes gets called for new buffers while they are still in
  ;;   `fundamental-mode', before a major mode has been assigned. I don't know
  ;;   why this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)

  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`doom-escape-hook' hooks."
      (ignore (or (memq this-command '(git-gutter:stage-hunk
                                       git-gutter:revert-hunk))
                  inhibit-redisplay
                  (if git-gutter-mode
                      (git-gutter)
                    (+vc-gutter-init-maybe-h))))))
  ;; UX: update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  ;; FIX: stop git-gutter:{next,previous}-hunk from jumping to random hunks.
  (defadvice! +vc-gutter--fix-linearity-of-hunks-a (diffinfos is-reverse)
    :override #'git-gutter:search-near-diff-index
    (cl-position-if (let ((lineno (line-number-at-pos))
                          (fn (if is-reverse #'> #'<)))
                      (lambda (line) (funcall fn lineno line)))
                    diffinfos
                    :key #'git-gutter-hunk-start-line
                    :from-end is-reverse)))


;;
;;; diff-hl

(use-package! diff-hl
  :when (modulep! +diff-hl)
  :hook (find-file    . diff-hl-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :commands diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk
  :config
  (set-popup-rule! "^\\*diff-hl" :select nil :size '+popup-shrink-to-fit)

  ;; PERF: reduce load on remote
  (defvaralias 'diff-hl-disable-on-remote '+vc-gutter-in-remote-files)
  ;; PERF: A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ;; PERF: Slightly more conservative delay before updating the diff
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3

  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (setq diff-hl-show-staged-changes nil)

  ;; UX: Update diffs when it makes sense too, without being too slow
  (when (modulep! :editor evil)
    (map! :after diff-hl-show-hunk
          :map diff-hl-show-hunk-map
          :n "p" #'diff-hl-show-hunk-previous
          :n "n" #'diff-hl-show-hunk-next
          :n "c" #'diff-hl-show-hunk-copy-original-text
          :n "r" #'diff-hl-show-hunk-revert-hunk
          :n "[" #'diff-hl-show-hunk-previous
          :n "]" #'diff-hl-show-hunk-next
          :n "{" #'diff-hl-show-hunk-previous
          :n "}" #'diff-hl-show-hunk-next
          :n "S" #'diff-hl-show-hunk-stage-hunk))
  ;; UX: Refresh git-gutter on ESC or refocusing the Emacs frame.
  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Return nil to prevent shadowing other `doom-escape-hook' hooks."
      (ignore (or inhibit-redisplay
                  (and (or (bound-and-true-p diff-hl-mode)
                           (bound-and-true-p diff-hl-dir-mode))
                       (diff-hl-update-once))))))
  ;; UX: Update diff-hl when magit alters git state.
  (when (modulep! :tools magit)
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; FIX: The revert popup consumes 50% of the frame, whether or not you're
  ;;   reverting 2 lines or 20. This resizes the popup to match its contents.
  (defadvice! +vc-gutter--shrink-popup-a (fn &rest args)
    :around #'diff-hl-revert-hunk-1
    (letf! ((refine-mode diff-auto-refine-mode)
            (diff-auto-refine-mode t)
            (defun diff-refine-hunk ()
              (when refine-mode
                (funcall diff-refine-hunk))
              (shrink-window-if-larger-than-buffer)))
      (apply fn args)))

  ;; UX: Don't delete the current hunk's indicators while we're editing
  (when (modulep! :editor evil)
    (add-hook! 'diff-hl-flydiff-mode-hook
      (defun +vc-gutter-init-flydiff-mode-h ()
        (if (not diff-hl-flydiff-mode)
            (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
          (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))))

  ;; FIX: Reverting a hunk causes the cursor to be moved to an unexpected place,
  ;;   often far from the target hunk.
  (defadvice! +vc-gutter--save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    :around #'diff-hl-revert-hunk
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt)))))

(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'vi-tilde-fringe-mode)
