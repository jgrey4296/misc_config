;; utility

(defun +jg-tag-sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) jg-tag-global-tags))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )))
(defun +jg-tag-candidates ()
  " Given Candidates, colour them if they are assigned, then sort them  "
  (let* ((buffer-cand-tags (+jg-tag-get-buffer-tags))
         (global-tags jg-tag-global-tags))
    (if (not (hash-table-empty-p global-tags))
        (let* ((cand-keys (hash-table-keys global-tags))
               (cand-vals (hash-table-values global-tags))
               (cand-pairs (-zip cand-keys cand-vals))
               (maxTagLength (apply 'max (mapcar 'length cand-keys)))
               (maxTagAmount (apply 'max cand-vals))
               (bar-keys (+jg-text-make-bar-chart cand-pairs maxTagLength maxTagAmount))
               (display-pairs (-zip bar-keys cand-keys))
               (current-tags (org-get-tags nil t))
               (propertied-tags (cl-map 'list (lambda (candidate)
                                             (let ((candString (car candidate)))
                                               (if (-contains? current-tags (cdr candidate))
                                                   (progn (put-text-property 0 (length candString)
                                                                             'font-lock-face
                                                                             'rainbow-delimiters-depth-1-face
                                                                             candString)))
                                               `(,candString ,(cdr candidate)))) display-pairs))
               )
          (setq jg-tag-candidate-counts global-tags)
          (setq jg-tag-candidates-names (sort propertied-tags '+jg-tag-sort-candidates))
          )
      '()
      ))
  )
(defun +jg-tag-chart-tag-counts (counthash name)
  "Given a hashtable of counts, create a buffer with a bar chart of the counts"
  ;; (message "Charting: %s %s" counthash name)
  (let* ((hashPairs (-zip (hash-table-keys counthash) (hash-table-values counthash)))
         (sorted (sort hashPairs (lambda (a b) (> (cdr a) (cdr b)))))
         (maxTagLength (apply 'max (mapcar (lambda (x) (length (car x))) sorted)))
         (maxTagAmnt (apply 'max (mapcar (lambda (x) (cdr x)) sorted)))
         )
    ;;print them all out

    (with-temp-buffer-window "*Tags*"
                             nil
                             nil
                             ;; Todo: Expand this func to group and add org headings
                             (mapc (lambda (x) (princ (format "%s\n" x)))
                                   (+jg-text-make-bar-chart sorted maxTagLength maxTagAmnt))
                             )
    (+jg-tag-org-format-temp-buffer "*Tags*" name)
    )
  )
