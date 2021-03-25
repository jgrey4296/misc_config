;;; org/jg-org/+org-funcs.el -*- lexical-binding: t; -*-
;; Functions that work on org files/ interact with the outside

(defun +jg-org-open_link_in_buffer ()
  """ a util function to force links to be open in emacs  """
  (interactive)
  (org-open-at-point 'in-emacs)
  )
(defun +jg-org-open_link_externally ()
  """ Open a link, forcing it to be external to emacs """
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-open-at-point)))
(defun +jg-org-link-not-exists-p ()
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(clock comment comment-block footnote-definition
                           footnote-reference headline inline-src-block inlinetask
                           keyword link node-property planning src-block timestamp)
                   t))
         (type (org-element-property :type context))
         (path (org-element-property :path context)))
    (and (equal type "file") (not (file-exists-p path)))))
(defun +jg-org-link-hint-external (uri)
  """ Open a link, forcing it to be external to emacs """
  (let* ((ext (f-ext uri))
         (open-externally (if ext
                              (-contains? jg-org-external-file-link-types (downcase ext))
                            nil))
         (current-prefix-arg (if open-externally '(16) nil))
         )
    (condition-case nil
        (call-interactively 'org-open-at-point)
      (error (org-open-link-from-string uri))))
  )
(defun +jg-org-quicklook-link ()
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(clock comment comment-block footnote-definition
                           footnote-reference headline inline-src-block inlinetask
                           keyword link node-property planning src-block timestamp)
                   t))
         (type (org-element-property :type context))
         (path (org-element-property :path context)))
    (if (equal type "file")
        (call-process "qlmanage" nil 0 nil "-x" path)
      (message "Link not a file"))))

(defun +jg-org-open-selection (pair)
  "Open only a selection of a large file "
  (let ((file (car pair))
        (selection-size (cdr pair))
        selection)
    (with-temp-buffer
      (insert-file file)
      (goto-char (random (- (point-max) selection-size)))
      (setq selection (buffer-substring (point) (+ (point) selection-size)))
      )
    (with-temp-buffer-window (format "*%s - selection*" (-last-item (f-split file)))
                             nil nil
                             (princ selection)
                             )
    )
  )
(defun +jg-org-chop-long-file (name &optional preferred-length)
  "Take long org files and split them into multiple files
If preferred-length is not specified, use jg-org-preferred-linecount
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
        (if (> linecount (or preferred-length jg-org-preferred-linecount))
            (setq linecount 0
                  count (+ 1 count))
          )
        )
      (append-to-file "\n** " nil (funcall fn-fn))
      (append-to-file last-position (point-max) (funcall fn-fn))
      )
    )
  )
(defun +jg-org-move-links ()
  " Go through all links in a file,
and either copy, or move, the referenced file to a new location
Prefix-arg to move the file otherwise copy it
"
  (interactive)
  ;;Specify target, or use default
  (let ((target (read-directory-name "Move To: "
                                     jg-org-link-move-base))
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

