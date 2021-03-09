;;; util/jg-tag/+ivy_actions.el -*- lexical-binding: t; -*-

;; Adds actions to ivy for easy search + tagging of files from dired
(defvar jg-ivy-registered-tag nil)

(defun jg-ivy-tag-set (x)
  " Register a tag to reuse "
  (message "Registering")
  (setq jg-ivy-registered-tag (read-string "Store Tag: "))
  )

(defun jg-ivy-tag-clear (x)
  " Clear the registered tag "
  (message "Clearing")
  (setq jg-ivy-registered-tag nil)
  )

(defun jg-ivy-set-tags (tag)
  (goto-char (line-beginning-position))
  (if (re-search-forward "^\*\* Thread:.+?\s+\\(:.+?:\\)$" (line-end-position) t)
        (let* ((match (match-data))
               (the-match (match-string-no-properties 1))
               (tag-set (make-hash-table :test 'equal))
               (tags (if the-match (split-string the-match ":" t "\s+") nil))
               replacement)

          (puthash tag 1 tag-set)
          (mapc #'(lambda (x) (puthash x 1 tag-set)) tags)
          (setq replacement (format ":%s:" (string-join (hash-table-keys tag-set) ":")))
          (set-match-data match)
          (replace-match replacement nil nil nil 1)
          )
    (progn
      (goto-char (line-end-position))
      (insert "          " ":" tag ":"))))

(defun jg-ivy-tag (x)
  "Opens the current candidate in another window."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let* ((file-name   (match-string-no-properties 1 x))
           (line-number (string-to-number (match-string-no-properties 2 x)))
           (full-file (expand-file-name file-name (ivy-state-directory ivy-last)))
           (input (plist-get (plist-get (ivy-state-extra-props ivy-last) :ivy-data) :text))
           (the-tag (if (not jg-ivy-registered-tag) (read-string "Tag as: ") jg-ivy-registered-tag))
          )
      (message "Using Tag: %s" the-tag)
      (with-temp-buffer
        ;; open the file indirectly
        (insert-file-contents full-file t)
        ;; go to the match
        (goto-char (point-min))
        (forward-line line-number)
        ;; go up to its thread header
        (if (re-search-backward "^\*\* Thread:" nil t)
            (progn
              ;; Set Tags
              (jg-ivy-set-tags the-tag)
              ;; Save the file
              (write-file full-file))
          )
        )
      )
    )
  )

(ivy-set-actions 'counsel-rg
                 '(("t" jg-ivy-tag "Tag")
                   ("T" jg-ivy-tag-set "Set Tag")
                   ("C" jg-ivy-tag-clear "Clear Tag")))
