
;;; emacs/jg-vc/autoload/advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc-gutter-type-at-pos-fn (type _pos)
  (if (eq type 'delete)
      'diff-hl-bmp-delete
    'diff-hl-bmp-middle))

;;;###autoload
(defun +vc-support-git-timemachine-a (fn)
  "Allow `browse-at-remote' commands in git-timemachine buffers to open that
file in your browser at the visited revision."
  (if git-timemachine-mode
      (let* ((start-line (line-number-at-pos (min (region-beginning) (region-end))))
             (end-line (line-number-at-pos (max (region-beginning) (region-end))))
             (remote-ref (browse-at-remote--remote-ref buffer-file-name))
             (remote (car remote-ref))
             (ref (car git-timemachine-revision))
             (relname
              (file-relative-name
               buffer-file-name (expand-file-name (vc-git-root buffer-file-name))))
             (target-repo (browse-at-remote--get-url-from-remote remote))
             (remote-type (browse-at-remote--get-remote-type target-repo))
             (repo-url (cdr target-repo))
             (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
        (unless url-formatter
          (error (format "Origin repo parsing failed: %s" repo-url)))
        (funcall url-formatter repo-url ref relname
                 (if start-line start-line)
                 (if (and end-line (not (equal start-line end-line))) end-line)))
    (funcall fn)))

;;;###autoload
(defun +vc-update-header-line-a (revision)
  "Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting revision
info in the `header-line-format' is a more visible indicator."
  (let* ((date-relative (nth 3 revision))
         (date-full (nth 4 revision))
         (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
         (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
    (setq header-line-format
          (format "%s%s [%s (%s)]"
                  (propertize author 'face 'git-timemachine-minibuffer-author-face)
                  (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                  date-full date-relative))))

;;;###autoload
(defadvice! +vc--fallback-to-master-branch-a ()
  "Return 'master' in detached state."
  ;; HACK `browse-at-remote' produces urls with `nil' in them, when the repo is
  ;;      detached. This creates broken links. I think it is more sensible to
  ;;      fall back to master in those cases.
  "master")

;;;###autoload
(defun +magit-revert-repo-buffers-deferred-a (&rest _)
  ;; Since the project likely now contains new files, best we undo the
  ;; projectile cache so it can be regenerated later.
  (projectile-invalidate-cache nil)
  ;; Use a more efficient strategy to auto-revert buffers whose git state has
  ;; changed: refresh the visible buffers immediately...
  (+magit-mark-stale-buffers-h)
  )

;;;###autoload
(defun +magit--forge-get-repository-lazily-a (&rest _)
  "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."

  (file-executable-p emacsql-sqlite-executable))

;;;###autoload
(defun +magit--forge-build-binary-lazily-a (&rest _)
  "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
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

;;;###autoload
(defun +vc-gutter--fix-linearity-of-hunks-a (diffinfos is-reverse)
  ;; FIX: stop git-gutter:{next,previous}-hunk from jumping to random hunks.
  (cl-position-if (let ((lineno (line-number-at-pos))
                        (fn (if is-reverse #'> #'<)))
                    (lambda (line) (funcall fn lineno line)))
                  diffinfos
                  :key #'git-gutter-hunk-start-line
                  :from-end is-reverse))

;;;###autoload
(defun +vc-gutter-define-thin-bitmaps-a (&rest args)
  (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
