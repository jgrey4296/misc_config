;;; +urls.el -*- lexical-binding: t; -*-
(require 'bibtex)

;;;###autoload
(defun +jg-bibtex--url-matcher (x)
  (when (and (string-match "url" (car x))
             (<= (length (cdr x)) 30))
    (cons (car x) (substring (cdr x) 1 -1)))
  )

;;;###autoload
(defun +jg-bibtex--expand-shortened-url ()
  "Expand a shortened url, using CuRL
https://tecnoysoft.com/en/how-to-obtain-the-real-url-behind-a-shortened-url-using-curl/
 "
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (urls (-reject #'(lambda (x) (or (null x) (string-equal (cdr x) ""))) (mapcar #'+jg-bibtex--url-matcher entry)))
         (result-buffer (get-buffer-create "*CurlResponse*"))
         expanded
         )
    (when urls (message "Expanding %s urls" (length urls)))
    (cl-loop for urlpair in urls
             do
             (with-current-buffer result-buffer
               (erase-buffer))
             (apply #'call-process jg-bibtex-curl-cmd nil result-buffer nil (append jg-bibtex-curl-args (ensure-list (cdr urlpair))))
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (when (re-search-forward "^location: " nil t)
                 (push (cons (car urlpair)
                             (s-replace "\r" "" (buffer-substring (point) (line-end-position))))
                       expanded))
               )
             )
    (cl-loop for urlpair in expanded
             do
             (bibtex-set-field (car urlpair) (cdr urlpair))
             )
    )
  )
