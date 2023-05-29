;;; lang/jg-org/+text-utils.el -*- lexical-binding: t; -*-
;; Functions to operate on Org Text

;;;###autoload
(defun +jg-org-insert-heading-trio ()
  (interactive)
  (org-insert-subheading 1)
  (insert "1: ")
  (org-insert-heading 3 nil nil)
  (insert "2: ")
  (org-insert-heading 1 nil nil)
  (insert "3: ")
  )

;;;###autoload
(defun +jg-org-change-link-name (name)
  " Change the name of a link "
  (interactive "s")
  (let ((re org-link-bracket-re))
    (save-excursion
      (beginning-of-line)
      (search-forward-regexp re (line-end-position))
      (replace-match name nil nil nil 2)
      )
    )
  )

;;;###autoload
(defun +jg-org-fill-paragraph-reverse ()
  " from END to START of buffer, fill paragraphs "
  (interactive)
  (goto-char (point-max))
  (let ((prog (lambda ()
                (let ((elem (cadr (org-element-at-point))))
                  (fill-region-as-paragraph
                   (cl-getf elem :begin) (cl-getf elem :end)))))
        curr-elem
        return-to
        )
    (while (not (eq (point) (point-min)))
      (setq curr-elem (org-element-at-point)
            return-to (cl-getf (cadr curr-elem) :begin))
      (cond ((eq (car curr-elem) 'property-drawer)
             (goto-char (- (cl-getf (cadr curr-elem) :begin) 1))
             )
            ((eq (car curr-elem) 'headline)
             (goto-char (- (cl-getf (cadr curr-elem) :begin) 1))
             )
            (t
             (goto-char (cl-getf (cadr curr-elem) :begin))
             (funcall prog)
             (insert "\n")
             )
            )
      (goto-char (- return-to 1))
      )
    )
  )

;;;###autoload
(defun +jg-org-refill-links ()
  ;;Find and replace
  (goto-char (point-min))
  (while (re-search-forward "]\\[\n[[:space:]]+" nil t)
    (replace-match "][")
    )
)

;;;###autoload
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

;;;###autoload
(defun +jg-org-wrap-numbers (a b)
  "Find numbers and wrap them in parentheses to avoid org treating them as lists"
  (interactive "r")
  (message "%s %s" a b )
  (goto-char a)
  (while (re-search-forward "^[[:space:]]*\\([[:digit:]]+\\)[.)] " b t)
    (replace-match "\n(\\1)")
    )
  )

;;;###autoload
(defun +jg-org-wrap-non-link-urls ()
  "Find urls that are not wrapped into org link format, and wrap them"
  (interactive)
  (let ((start (if (eq evil-state 'visual) evil-visual-beginning (point-min)))
        (last (if (eq evil-state 'visual) evil-visual-end  nil)))
    (goto-char start)
    (while (re-search-forward "[^[[]\\(http[^ …\n]+\\)" last t)
      (replace-match "[[\\1][ᵢ]]")
      )
    )
  )

;;;###autoload
(defun +jg-org-sort-headings ()
  " Call org-sort-entries on a buffer "
  (message "Sorting Headings")
  (goto-char (point-min))
  (org-mode)
  (org-show-all)
  (org-sort-entries nil ?a)
  )
