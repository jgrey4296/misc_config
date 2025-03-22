;;; actions.el -*- lexical-binding: t; -*-
(require 'ivy)

;;;###autoload
(defun +jg-ivy-kill-buffer (buff)
  (interactive)
  (with-current-buffer buff
    (kill-current-buffer)
    )
  )

;;;###autoload
(defun +jg-ivy-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.
Modified to pre-sort bookmarks, caselessly
"
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (cl-sort (bookmark-all-names) #'string-lessp :key #'downcase)
            :history 'bookmark-history
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let (
(default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

;;;###autoload
(defun +jg-ivy-features ()
  " Insert from a list of recognized features "
  (interactive)
  (ivy-read "Available Features: "
            (seq-sort #'string-lessp features)
            :require-match nil
            :action 'insert
            )
  )

;;;###autoload
(defun +jg-ivy-toggle-mark ()
  (interactive)
  (if (ivy--marked-p)
      (ivy--unmark (ivy-state-current ivy-last))
    (ivy--mark (ivy-state-current ivy-last)))
  (ivy-next-line)
  )

;;;###autoload
(defun +jg-ivy--action-insert (x)
  " Ivy Insert, but with spaces between inserts if adding multipl "
  (goto-char swiper--opoint)
  (insert (if (stringp x) (ivy--trim-grep-line-number x) x (car x))
          (if ivy-marked-candidates "\n" "")
          )
  )

;;;###autoload
(defun +jg-ivy--action-yank (x)
  " Kill the selected candidate and paste it where you started "
  (kill-region (line-beginning-position) (line-end-position))
  (goto-char swiper--opoint)
  (insert "|^|")
  (yank)
  )

;;;###autoload
(defun +jg-ivy--action-kill (x)
  "Kill the region of the selected candidate"
  (kill-region (line-beginning-position) (line-end-position))
  )

;;;###autoload
(defun +ivy-git-grep-other-window-action (x)
  "Opens the current candidate in another window."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (select-window
     (with-ivy-window
       (let ((file-name   (match-string-no-properties 1 x))
             (line-number (match-string-no-properties 2 x)))
         (find-file-other-window (expand-file-name file-name (ivy-state-directory ivy-last)))
         (goto-char (point-min))
         (forward-line (1- (string-to-number line-number)))
         (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
         (run-hooks 'counsel-grep-post-action-hook)
         (selected-window))))))

;;;###autoload
(defun +ivy/git-grep-other-window-action ()
  "Open the current counsel-{ag,rg,git-grep} candidate in other-window."
  (interactive)
  (ivy-set-action #'+ivy-git-grep-other-window-action)
  (setq ivy-exit 'done)
  (exit-minibuffer))
