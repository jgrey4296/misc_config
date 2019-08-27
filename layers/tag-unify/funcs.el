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

(defun tag-unify/open-url (x)


  )

(defun tag-unity/goto-entry (x)


  )
