;;; lang/org/autoload/org-babel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-eval-handler (beg end)
  " Test to see if in a src block, then eval it if so"
  (save-excursion
    (let ((in-src-block
           (cl-loop for pos in (list beg (point) end)
                    if (save-excursion (goto-char pos) (org-in-src-block-p t))
                    return (goto-char pos)))
          )
      (if (not in-src-block)
          (message "Nothing to evaluate at point")
        (let* ((element (org-element-at-point))
               (block-beg (save-excursion
                            (goto-char (org-babel-where-is-src-block-head element))
                            (line-beginning-position 2)))
               (block-end (save-excursion
                            (goto-char (org-element-property :end element))
                            (skip-chars-backward " \t\n")
                            (line-beginning-position)))
               (beg (if beg (max beg block-beg) block-beg))
               (end (if end (min end block-end) block-end))
               (lang (or (org-eldoc-get-src-lang)
                         (user-error "No lang specified for this src block")))
               (major-mode (org-src-get-lang-mode lang))
               )
          (+eval:region beg end)
          )
        )
      )
    )
  )

;;;###autoload
(defun +org-lookup-definition-handler (identifier)
  "When in a src block, use that language's lookup handlers"
  (when (org-src-block-p t)
    (user-error "TODO")
    )
  )

;;;###autoload
(defun +org-lookup-references-handler (identifier)
  "when in a src block, use that languages lookup handlers"
  (when (org-in-src-block-p t)
    (user-error "TODO")
    )
  )

;;;###autoload
(defun +org-lookup-documentation-handler (identifier)
  "when in a src block, use that languages lookup handlers"
  (when (org-in-src-block-p t)
    (user-error "TODO")
    )
  )

;;
;;; Commands

;;;###autoload
(defun +org/remove-result-blocks (remove-all)
  "Remove all result blocks located after current point."
  (interactive "P")
  (let ((pos (point)))
    (org-babel-map-src-blocks nil
      (if (or remove-all (< pos end-block))
          (org-babel-remove-result)))))

;;
;;; Hooks

;;;###autoload
(defun +org-clear-babel-results-h ()
  "Remove the results block for the org babel block at point."
  (when (and (org-in-src-block-p t)
             (org-babel-where-is-src-block-result))
    (org-babel-remove-result)
    t))
