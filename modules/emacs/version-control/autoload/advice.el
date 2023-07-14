;;; emacs/jg-vc/autoload/advice.el -*- lexical-binding: t; -*-

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
(advice-add 'browse-at-remote-get-url :around #'+vc-support-git-timemachine-a)

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
(advice-add 'git-timemachine--show-minibuffer-details :override #'+vc-update-header-line-a)

;;;###autoload
(defadvice! +vc--fallback-to-master-branch-a ()
    "Return 'master' in detached state."
    ;; HACK `browse-at-remote' produces urls with `nil' in them, when the repo is
    ;;      detached. This creates broken links. I think it is more sensible to
    ;;      fall back to master in those cases.
    "master")

;;;###autoload
(advice-add 'browse-at-remote--get-local-branch :after-until #'+vc--fallback-to-master-branch-a)
