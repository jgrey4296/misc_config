;;; +transformers.el -*- lexical-binding: t; -*-

;;-- candidate transformers

(defun +jg-tag-sort-by-files (candidates source)
  (sort candidates (lambda (a b)
                     (let ((a-count (plist-get (cdr a) :count))
                           (b-count (plist-get (cdr b) :count)))
                       (> (if (stringp a-count) (string-to-number a-count) a-count)
                          (if (stringp b-count) (string-to-number b-count) b-count))
                       ))))

(defun +jg-tag-helm-index-file-transformer (cands)
  (let* ((as-list (mapcar (lambda (x) (split-string x ":" t "\s+")) cands))
         (max-tag (apply 'max (mapcar (lambda (x) (length (car x))) as-list)))
         (has-count (and (car as-list) (cadr (car as-list)) (s-numeric? (cadr (car as-list))))))
    (mapcar (lambda (x)
              `(,(format "%s%s: %s" (car x) (s-repeat (+ 1 (- max-tag (length (car x)))) " ") (if has-count (cadr x)
                                                                                                (length (cdr x))))
                . (:count ,(if has-count (cadr x) (length (cdr x)))
                   :files ,(if has-count (cddr x) (cdr x)))))
            as-list)
    )
  )

(defun +jg-tag-grep-filter-one-by-one (candidate)
        "A Grep modification for bookmark helm to extract a bookmark's url and tags"
        (if (consp candidate)
            ;; Already computed do nothing (default as input).
            candidate
          (let* ((line   (ansi-color-apply candidate))
                 (split  (helm-grep-split-line line)))
            (if (and split (>= (length split) 2))
                (let* ((lineno (if (nth 1 split) (nth 1 split) "1"))                 ;; Normalize Size of this
                       (norm-ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
                       (str    (nth 2 split))                                        ;; The Actual Line:
                       (sub    str)                                                  ;;(substring str (or (s-index-of "HREF=" str) 0)))
                       (tag-index (s-index-of " :" sub))                             ;;(s-index-of "TAGS=\"" sub))
                       (url (substring sub 0 tag-index))                             ;;(string-width "HREF=\"") (- tag_index 2)))
                       (tags (substring sub (+ tag-index 2) nil))
                       (chopped_tags (substring tags 0 (min 100 (string-width tags)))) ;; Normalize the lengths of tags so urls are aligned
                       (norm-tags (s-append (s-repeat (- 100 (string-width chopped_tags)) " ") chopped_tags))
                       )
                  `(,(concat (propertize norm-ln 'face 'helm-grep-lineno)
                             (propertize (concat ": " norm-tags) 'face 'rainbow-delimiters-depth-3-face)
                             (propertize (concat ": " url) 'face 'rainbow-delimiters-depth-1-face))
                    :url ,url
                    :tags ,tags
                    :line ,line
                    )
                  )
              )
            )
          )
        )

(defun +jg-tag-twitter-grep-filter-one-by-one (candidate)
        "A Grep modification for twitter grep helm to extract information correctly "
        (if (consp candidate)
            ;; Already computed do nothing (default as input).
            candidate
          (let* ((line   (ansi-color-apply candidate))
                 (split  (helm-grep-split-line line))
                 ;; Normalize Size of this:
                 (lineno (if (nth 1 split) (nth 1 split) "1"))
                 (norm-ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
                 ;; The Actual Line:
                 (str    (nth 2 split))
                 (sub    str) ;;(substring str (or (s-index-of "HREF=" str) 0)))
                 (tag-index (s-index-of " :" sub)) ;;(s-index-of "TAGS=\"" sub))
                 (tag (substring sub 0 tag-index)) ;;(string-width "HREF=\"") (- tag_index 2)))
                 (file-str (substring sub (+ tag-index 2) nil))
                 (file-list (split-string file-str ":" t "\s+"))
                 (has-count (s-numeric? (car file-list)))
                 (file-count (if has-count (string-to-number (car file-list)) (length file-list)))
                 (files (if has-count (cdr file-list) file-list))
                 ;; Normalize the lengths of tags so urls are aligned
                 (chopped_files (substring file-str (min 100 (string-width file-str))))
                 (norm-files (s-append (s-repeat (- 100 (string-width chopped_files)) " ") chopped_files))
                 )
            `(,(concat (propertize norm-ln 'face 'helm-grep-lineno)
                       (propertize (concat ": " tag) 'face 'rainbow-delimiters-depth-3-face)
                       (propertize (concat ": " (number-to-string file-count)) 'face 'rainbow-delimiters-depth-4-face))
              . (:count ,(if has-count (string-to-number (car file-list)) (length file-list))
                 :files ,(if has-count (cdr file-list) file-list))
              )
            )
          )
        )
;;-- end candidate transformers

;;-- pattern transformers

(defun +jg-tag-grep-pattern-transformer (pattern)
  (format "^[^:]*%s[^:]* :" pattern))
;;-- end pattern transformers
