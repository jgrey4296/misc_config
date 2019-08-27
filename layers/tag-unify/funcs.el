(defun tag-unify/process-candidates (x)
  (cons (s-replace-regexp ",? +" " " (car x))
        (cdr x))
  )
(defun tag-unify/build-bibtex-list ()
  (mapcar (lambda (x) (f-join tag-unify/loc-bibtex x))
          (-filter (lambda (x) (s-equals? (f-ext x) "bib"))
                   (directory-files tag-unify/loc-bibtex))))
(defun tag-unify/rebuild-tag-database ()
  ;; regenerate the master list of tags
  ;; from bookmarks



  ;; and from bibtex





  )
(defun tag-unify/get-bibtex-entries (x)
  ;;given a candidate tag name,
  ;;find all bibtex entries that match
  )
(defun tag-unify/load-bookmark-entry (x)
  ;;given a candidate tag name,
  ;;find all bookmark entries that match



  )
(defun tag-unify/open-url-action (x)
  """ An action added to helm-grep for loading urls found
  in bookmarks "
  (interactive)
  (let* ((marked (helm-marked-candidates))
         (no-props (mapcar (lambda (x) (substring-no-properties x 0 (length x))) marked))
         link-start-point
         )
    (with-temp-buffer
      (mapcar (lambda (x) (insert (format "%s\n" x))) no-props)
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (search-forward "href=\"")
        (jg_layer/open_link_externally)
        ;; (setq link-start-point (point))
        ;; (search-forward "\" TAGS")
        ;; (backward-char (length "\" TAGS"))
        (forward-line)
        )
      )
    )
  )
(defun tag-unity/goto-entry (x)


  )
