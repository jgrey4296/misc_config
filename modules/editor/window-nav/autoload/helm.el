;;; helm.el -*- lexical-binding: t; -*-
(require 'helm-source)
(require 'helm-grep)
(require 'helm-utils)
(require 'helm-files)

(defvar jg-nav-bookmark-helm-source
      (helm-make-source "Bookmark Helm" 'helm-grep-class
        :action (helm-make-actions "Open Url" #'+jg-nav-open-url-action
                                   "Insert"   #'+jg-nav-insert-bookmarks
                                   "Insert Link" #'+jg-nav-insert-links
                                   "Tweet Link"  #'+jg-nav-tweet-link-action
                                   )
        :filter-one-by-one   #'+jg-nav-grep-filter-one-by-one
        :pattern-transformer #'+jg-nav-bookmark-helm-pattern-transformer
        :nomark nil
        :backend (format "%s --color=always -a -d %s"
                         (pcase system-type
                           ('darwin "ggrep")
                           ('gnu/linux "grep")
                           )
                         "skip %e -n%cH -e %p %f"
                         )
        :pcre nil
        )
      )

(defun +jg-nav-bookmark-helm-pattern-transformer (pattern)
  pattern
)

(defun +jg-nav-open-url-action (x)
  " An action added to helm-grep for loading urls found in bookmarks "
  (let* ((marked (helm-marked-candidates))
         (no-props (mapcar (lambda (x) (plist-get x :url)) marked))
         link-start-point
         )
    (with-temp-buffer
      (org-mode)
      (mapc (lambda (x) (insert (format "%s\n" x))) no-props)
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (message "Opening: %s" (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-beginning-position))
        (+jg-org-open_link_externally)
        (forward-line)
        )
      )
    )
  )

(defun +jg-nav-tweet-link-action (candidate)
  "Helm action to open a tweet buffer with the link inserted"
  (evil-window-new (get-buffer-window helm-current-buffer)
                   "*Link Tweeting*")
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (evil-window-set-height 10)
  (evil-initialize-local-keymaps)
  (evil-local-set-key 'normal
                      (kbd "C-c C-C") '+jg-nav-tweet-link-finish)
  (insert "\n")
  (insert (plist-get candidate :url))
  (redraw-display)
  )

(defun +jg-nav-tweet-link-finish ()
  "Action to finish and tweet a link"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (+jg-twitter-twitter-tweet-text text nil '(+jg-twitter-tweet_sentinel))
    ))

(defun +jg-nav-insert-bookmarks (x)
 (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (plist-get x :url))
                         candidates "\n\n")))))

(defun +jg-nav-insert-links (x)
  "Helm action to insert selected candidates formatted as org links"
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]    : %s" (plist-get x :url) (plist-get x :url) (plist-get x :tags))) candidates "\n")))))

(defun +jg-nav-grep-filter-one-by-one (candidate)
        "A Grep modification for bookmark helm to extract a bookmark's url and tags"
        (if (consp candidate)
            ;; Already computed do nothing (default as input).
            candidate
          (let* ((line   (ansi-color-apply candidate))
                 (split  (helm-grep-split-line line)))
            (if (and split (>= (length split) 2))
                (let* ((lineno (if (nth 1 split) (nth 1 split) "1"))                 ;; Normalize Size of this
                       (norm-ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
                       (str    (nth 2 split))                                        ;; The Actual Line:
                       (sub    str)                                                  ;;(substring str (or (s-index-of "HREF=" str) 0)))
                       (tag-index (s-index-of " :" sub))                             ;;(s-index-of "TAGS=\"" sub))
                       (url (substring sub 0 tag-index))                             ;;(string-width "HREF=\"") (- tag_index 2)))
                       (tags (substring sub (+ tag-index 2) nil))
                       (chopped_tags (substring tags 0 (min 100 (string-width tags)))) ;; Normalize the lengths of tags so urls are aligned
                       (norm-tags (s-append (s-repeat (- 100 (string-width chopped_tags)) " ") chopped_tags))
                       )
                  `(,(concat (propertize norm-ln 'face 'helm-grep-lineno)
                             (propertize (concat ": " norm-tags) 'face 'rainbow-delimiters-depth-3-face)
                             (propertize (concat ": " url) 'face 'rainbow-delimiters-depth-1-face))
                    :url ,url
                    :tags ,tags
                    :line ,line
                    )
                  )
              )
            )
          )
        )

;;;###autoload
(defun +jg-nav-helm-bookmarks ()
    " Run a Helm for search and opening html bookmarks "
    (interactive)
    (helm-set-local-variable
     'helm-grep-last-targets `(,jg-nav-loc-bookmarks)
     'default-directory (f-parent jg-nav-loc-bookmarks)
     'helm-grep-default-command (alist-get 'backend jg-nav-bookmark-helm-source)
     )
    (helm :sources jg-nav-bookmark-helm-source
          :full-frame t
          :buffer "*helm bookmarks*"
          :truncate-lines t
          )
    )
