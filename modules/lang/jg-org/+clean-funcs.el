;; org cleaning

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

(defun +jg-org-clean (&optional skipfck)
  "The Main Clean-org routine:
Wrap Lines
Indent
Fill-paragraphs end -> beginning
Cleanup Whitespace
Wrap twitter picture urls
Re-clean whitespace
Tidy Link positions
Indent + Whitespace + new lines

Optionally remove duplicates of links
"
  (interactive)
  (message "Starting Org Clean")
  (message "Hiding Properties")
  ;; indent region
  ;;wrap lines that are too long
  (goto-char (point-min))
  (+jg-org-wrap-non-link-urls)
  (indent-region (point-min) (point-max))
  (+jg-org-fill-paragraph-reverse)
  (whitespace-cleanup)
  (org-show-all)
  ;;Find all pic.twitter's and ensure on new line
  (goto-char (point-min))
  (message "Finding pic.twitter's")
  (while (search-forward "pic.twitter" nil t)
    (let ((sub (buffer-substring (line-beginning-position) (point))))
      (if (not (eq 0 (string-match "^[[:space:]]+pic.twitter" sub)))
          (progn
            (backward-char (+ 1 (length "pic.twitter")))
            (insert "\n\n")))
      (progn (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring (line-beginning-position -0)
                                                                           (line-end-position -0))))
               (join-line)
               )
             (goto-char (line-beginning-position))
             (insert "\n")
             (forward-line))
      )
    )
  ;; Clean Whitespace
  (message "Cleaning Whitespace")
  (setq jg-org-clean-marker (make-marker))
  (org-map-entries '+jg-org-map-entries-clean-whitespace t nil)
  (set-marker jg-org-clean-marker nil)

  ;; Tidy all links:
  ;; DO NOT USE ORG-NEXT-LINK
  ;; it ignores links in property drawers
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil t)
    (set-marker jg-org-clean-marker (point))
    (goto-char (car (match-data 0)))
    (let ((prev-line (buffer-substring (line-beginning-position 0)
                                       (line-end-position 0))))
      (cond  ((eq 0 (string-match "^[[:space:]]+:PERMALINK:" prev-line))
              (join-line))
             ((eq 0 (string-match "^[[:space:]]+:PROPERTIES:" prev-line))
              nil)
             ((eq 0 (string-match "^[[:space:]]+:URL: " (buffer-substring (line-beginning-position) (point))))
              nil)
             ((not (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                         (line-beginning-position)
                                                         (point)))))
              (insert "\n")
              )
             (t
              (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                           (line-beginning-position 0)
                                                           (line-end-position 0))))
                (join-line)
                )
              )
             )
      )
    (goto-char jg-org-clean-marker)
    )

  (+jg-org-clean-property-blocks)
  (message "Indenting")
  (indent-region (point-min) (point-max))
  (whitespace-cleanup)
  (setq jg-org-clean-marker nil)

  ;;Find and replace
  (goto-char (point-min))
  (while (re-search-forward "]\\[\n[[:space:]]+" nil t)
    (replace-match "][")
    )

  (if (not skipfck)
      (progn
        (goto-char (point-min))
        (while (re-search-forward  "file:.+?%.+$" nil t)
          (if (+jg-org-link-not-exists-p)
              (progn
                (goto-char (line-beginning-position))
                (insert "--->")
                (if (s-equals? (read-string "Delete line? ") "y")
                    (delete-region (line-beginning-position) (line-end-position))
                  (progn
                    (delete-region (line-beginning-position)
                                   (+ (line-beginning-position) (length "--->")))
                    (forward-line))
                  ))
            )
          )
        )
    )

  (org-cycle-hide-drawers 'all)
  (goto-char (point-min))
  (message "Org Clean Finished")
  )
(defun +jg-org-clean-with-backup (name)
  "A Wrapper around clean org to back up the original file "
  (message "----------")
  (message "Cleaning: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (write-file (format "%s_orig" name))
    (org-mode)
    (+jg-org-clean t)
    (write-file name)
    )
  (message "Finished Cleaning")
  )

(defun +jg-org-map-entries-clean-whitespace ()
  "Called from org-map-entries. reduces whitespace prior
to point to a single new line"
  (set-marker jg-org-clean-marker (line-end-position))
  (if (not (eq (point) (point-min)))
      (progn
        (while (eq 0 (string-match "^[[:space:]]*$"
                                   (buffer-substring
                                    (line-beginning-position 0)
                                    (line-end-position 0))))
          (join-line))
        (if (not (string-equal "*" (buffer-substring
                                    (line-beginning-position 0)
                                    (+ 1 (line-beginning-position 0)))))
            (insert "\n"))
        (setq org-map-continue-from jg-org-clean-marker)
        )
    )
  )

(defun +jg-org-clean-property-blocks ()
  " Find Property blocks and clean newlines in them "
  ;; TODO merge with +jg-org-fix-properties-drawers
  (goto-char (point-min))
  ;; Find properties block
  (let ((start-marker (make-marker))
        (end-marker (make-marker)))
  (while (search-forward-regexp ":PROPERTIES:" nil t)
    (set-marker start-marker (point))
    (search-forward ":END:" nil t)
    (goto-char (nth 0 (match-data)))
    (set-marker end-marker (point))
    (goto-char start-marker)
    (while (< (point) end-marker)
      (join-line 1)
      )
    (goto-char start-marker)
    ;; Loop over adding new lines appropriately
    (while (and (< (point) end-marker) (search-forward-regexp "\\(:[_A-Z]+:\\)" nil t))
      (replace-match "\n\\&")
      )
    )
  )
  )
