;;; org/jg-org/+org-funcs.el -*- lexical-binding: t; -*-
;; Functions that work on org files/ interact with the outside

;;-- utils

;;;###autoload
(defun +jg-org-open_link_in_buffer ()
  " a util function to force links to be open in emacs  "
  (interactive)
  (org-open-at-point 'in-emacs)
  )

;;;###autoload
(defun +jg-org-open_link_externally ()
  " Open a link, forcing it to be external to emacs "
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-open-at-point)))

;;;###autoload
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

;;;###autoload
(defun +jg-org-link-hint-external (uri)
  " Open a link, forcing it to be external to emacs "
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun +jg-org-list-agenda-files ()
  " Creates a temporary, Org-mode buffer with links to agenda files "
  (interactive)
  (with-output-to-temp-buffer "*Agenda Files*"
    (set-buffer "*Agenda Files*")
    (insert "Agenda Files: ")
    (insert "\n")
    (mapc (lambda (x)
            (let ((file_name (last (split-string x "/" t ".org"))))
              (insert (format "[[%s][%s]]\n" x file_name))
              )) org-agenda-files)
    (org-mode)
    )
  )

;;;###autoload
(defun +jg-org-split-on-headings ()
  " Split an org file into multiple smaller buffers non-destructively "
  (interactive)
  (let ((contents (buffer-substring (point-min) (point-max)))
        (target-depth (read-number "What Depth Subtrees to Copy? "))
        (target-dir (read-directory-name "Split into directory: "))
        (map-fn (lambda ()
                  (let* ((components (org-heading-components))
                         (depth (car components)))
                    ;;Only copy correct depths
                    (if (eq depth target-depth)
                        (progn
                          ;; (message (format "Current : %s %s" count (nth 4 components)))
                          (org-copy-subtree 1)
                          (current-kill 0 t)
                          )
                      )
                    )
                  ))
        results
        )
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (goto-char (point-min))
      (setq results (-non-nil (org-map-entries map-fn)))
      )
    (-each (-zip-fill target-dir results '()) '+jg-text-org-split-temp-buffer-create)
    )
  )

;;;###autoload
(defun +jg-org-split-alphabetically ()
  " Split a buffer of values on separate lines into headings alphabetically "
  (interactive)
  (goto-char (point-min))
  (let ((current ?a)
        (end ?z))
    (insert "* Top\n")
    (while (and (<= current end)
                (re-search-forward (format "^%c" current)))
      (goto-char (line-beginning-position))
      (insert (format "** %c\n" current))
      (cl-incf current)
      )
    )
  )

;;;###autoload
(defun +jg-org-split-tag-list ()
  " Combine the org-split functions into a single routine.
Sort, align, split, save "
  (interactive)
  (let ((text (buffer-string))
        (sort-fold-case t))
    (with-temp-buffer
      (insert text)
      (sort-lines nil (point-min) (point-max))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):" 1 nil t)
      (jg-tag-org-split-alphabetically)
      (jg-tag-org-split-on-headings)
      )
    )
  )

;;;###autoload
(defun +jg-org-refile-subtree (arg)
  ;; based on +org/refile-to-other-window
  "TODO"
  (interactive "P")
  (let ((org-refile-keep arg)
        (target (read-file-name "Refile to: " jg-org-twitter-loc nil nil nil))

        org-refile-targets
        current-prefix-arg
        )
    (cl-pushnew (cons target (cons :maxlevel 10))
                org-refile-targets)
    (call-interactively #'org-refile)

    )
  )
;;-- end utils

;;;###autoload
(defun +jg-org-clean-heading-spaces ()
  (org-map-entries #'(lambda ()
                       (forward-line -1)
                       (end-of-line)
                       (insert "\n")
                       (forward-line 2)
                       (setq org-map-continue-from (line-end-position))
                       )
                   t
                   nil)
  )

;;;###autoload
(defun +jg-org-inline-image-override (link &optional rules)
  "Override function for org export, to succeed on file links with descriptions"
  (let ((case-fold-search t))
    (cl-some (lambda (rule)
	       (and (string= (org-element-property :type link) (car rule))
		    (string-match-p (cdr rule)
				    (org-element-property :path link))))
	     (or rules org-export-default-inline-image-rule)))
  )

;;;###autoload
(advice-add 'org-export-inline-image-p :override #'+jg-org-inline-image-override)
