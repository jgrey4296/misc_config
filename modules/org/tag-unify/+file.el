;; file processing
(provide 'jgul-file)

(defun jg-tag-unify-layer/chop-long-file (name &optional preferred-length)
  "Take long org files and split them into multiple files
If preferred-length is not specified, use jg-tag-unify-layer/preferred-linecount-for-org
"
  (message "----------")
  (message "Chopping: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (let* ((count 1)
           (base_name (file-name-sans-extension name))
           (internal_name (buffer-substring (+ 2 (point-min)) (line-end-position)))
           (master_name (format "%s_master.org" base_name))
           (regexp "^\\*\\*[^*]")
           (last-position (re-search-forward regexp nil t))
           (linecount 0)
           (fn-fn (lambda () (format "%s_%s.org" base_name count)))
           (ln-fn (lambda (a b) (- (line-number-at-pos (max a b))
                                   (line-number-at-pos (min a b)))))
           )
      (append-to-file (format "* %s\n" internal_name) nil master_name)
      (while (re-search-forward "^\\*\\*[^*]" nil t )
        (if (not (file-exists-p (funcall fn-fn)))
            (progn (message "Creating %s" (funcall fn-fn))
                   (append-to-file (format "* %s %s\n" internal_name count) nil (funcall fn-fn))
                   (append-to-file (format "** [[%s][%s %s]]\n" (funcall fn-fn) internal_name count) nil master_name)
                   )
          )
        (append-to-file "\n** " nil (funcall fn-fn))
        (append-to-file last-position (line-beginning-position) (funcall fn-fn))
        (setq linecount (+ linecount (funcall ln-fn (point) last-position))
              last-position (point))
        (if (> linecount (or preferred-length jg-tag-unify-layer/preferred-linecount-for-org))
            (setq linecount 0
                  count (+ 1 count))
          )
        )
      (append-to-file "\n** " nil (funcall fn-fn))
      (append-to-file last-position (point-max) (funcall fn-fn))
      )
    )
  )
(defun jg-tag-unify-layer/chop-long-files-from-dired ()
  "Subdivide marked files if they are too long"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/chop-long-file files)
    )
  )
(defun jg-tag-unify-layer/move-links ()
  " Go through all links in a file,
and either copy, or move, the the referenced file to a new location
Prefix-arg to move the file otherwise copy it
"
  (interactive)
  ;;Specify target, or use default
  (let ((target (read-directory-name "Move To: "
                                     "/Volumes/Overflow/missing_images/"))
        (action (if current-prefix-arg 'rename-file 'copy-file))
        link
        )
    (if (not (file-exists-p target))
        (progn (message "Making target directory: %s" target)
               (mkdir target))
      )
    (message "Process Type: %s" action)
    (goto-char (point-min))
    (while (eq t (org-next-link))
      (setq link (plist-get (plist-get (org-element-context) 'link) :path))
      (message "Processing: %s" link)
      (if (not (f-exists? (f-join target (-last-item (f-split link)))))
          (funcall action link target)
        (message "Already Exists"))
      )
    )
  )

