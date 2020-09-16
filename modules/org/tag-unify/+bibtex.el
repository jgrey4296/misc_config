;; bibtex
(provide 'jgul-bibtex)

(defun jg-tag-unify-layer/build-bibtex-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (setq bibtex-completion-bibliography (directory-files jg-tag-unify-layer/loc-bibtex 't "\.bib$")))

(defun jg-tag-unify-layer/bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg-tag-unify-layer/jg-tag-unify-layer-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-pos jg-tag-unify-layer/jg-tag-unify-layer-marker)
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-unify-layer/global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-unify-layer/global-tags) 1) jg-tag-unify-layer/global-tags))
                       )))
         )
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn (setq current-tags (split-string (bibtex-autokey-get-field "tags") "," t " +")
                     prior-point (point))
               (mapc add-func actual-candidates)
               (bibtex-set-field "tags" (string-join current-tags ","))
               (org-ref-bibtex-next-entry)
               )))
    )
  )
(defun jg-tag-unify-layer/bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos jg-tag-unify-layer/jg-tag-unify-layer-marker)
          (stripped_tag (jg-tag-unify-layer/strip_spaces x))
          )
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (setq prior-point (point))
        (let* ((current-tags (split-string (bibtex-autokey-get-field "tags") "," t " +")))
          (if (not (-contains? current-tags stripped_tag))
              (progn
                (push stripped_tag current-tags)
                (puthash stripped_tag 1 jg-tag-unify-layer/global-tags)))
          (bibtex-set-field "tags" (string-join current-tags ","))
          (org-ref-bibtex-next-entry)
          ))))
  )
(defun jg-tag-unify-layer/unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward "file[[:digit:]]* ?= *{\\(.+mega\\)/\\(mendeley\\)?" nil t)
      (replace-match "~/Mega" nil nil nil 1)
      (if (eq 6 (length (match-data)))
          (replace-match "pdflibrary" t nil nil 2))
      )
    (write-file name)
    )
  )
(defun jg-tag-unify-layer/unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/unify-pdf-locations-in-file files)
    )
  )

