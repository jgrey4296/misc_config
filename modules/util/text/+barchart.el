;;; util/+jg-text/+barchar.el -*- lexical-binding: t; -*-


(defun +jg-text-make-bar-chart (data maxTagLength maxTagAmnt)
  " Make a bar chart from passed in hashtable and descriptive information "
  (let* ((maxTagStrLen (length (number-to-string maxTagAmnt)))
         (maxTagLength-bounded (min 40 maxTagLength))
         (max-column (- fill-column (+ 3 maxTagLength-bounded maxTagStrLen 3 3)))
         (bar-div (/ (float max-column) maxTagAmnt)))
    (mapcar '+jg-text-bar-chart-line data)))
(defun +jg-text-bar-chart-line (x)
  "Construct a single line of a bar chart"
  (let* ((tag (car x))
         (tag-len (length tag))
         (tag-cut-len (- maxTagLength-bounded 3))
         (tag-truncated-p (> tag-len maxTagLength-bounded))
         (tag-substr (if tag-truncated-p (string-join `(,(substring tag nil tag-cut-len) "..."))
                       tag))
         (tag-final-len (length tag-substr))
         (amount (cdr x))
         (amount-str (number-to-string amount))
         (sep-offset (- (+ 3 maxTagLength-bounded) tag-final-len))
         (amount-offset (- maxTagStrLen (length amount-str)))
         (bar-len (ceiling (* bar-div amount)))
         )
    (string-join `(,tag-substr
                   ,(make-string sep-offset ?\ )
                   " : "
                   ,amount-str
                   ,(make-string amount-offset ?\ )
                   " : "
                   ,(make-string bar-len ?=)
                   ;; "\n"
                   )))
  )
(defun +jg-text-barchart-region()
  " Create a Bar Chart of value pairs of the selected region "
  (interactive)
  (assert (eq evil-state 'visual))
  (let* (;;grab the region
         (text (buffer-substring-no-properties evil-visual-beginning
                                               evil-visual-end))
         ;;split text into pairs
         (lines (split-string text "\n" t " +"))
         (pairs (mapcar (lambda (x) (split-string x ":" t " +")) lines))
         (count-hash (make-hash-table :test 'equal))
         )
    ;; (message "Getting Tags for all buffers to depth: %s" depth)
    (mapcar (lambda (x) (cl-incf (gethash (car x) count-hash 0) (string-to-number (cadr x)))) pairs)
    (if (not (hash-table-empty-p count-hash))
        (+jg-text-chart-tag-counts count-hash (buffer-name))
      (message "No Tags in buffer")))
  )
(defun +jg-text-org-format-temp-buffer (name source_name)
  " Format bar chart buffer as an org buffer.
Adds a header, separates similar counted lines into sub headings,
and sorts groups alphabetically"
  (with-current-buffer name
    (org-mode)
    (let ((inhibit-read-only 't)
          (last_num nil)
          (get_num_re ": \\([[:digit:]]+\\) +:")
          (start-marker (make-marker))
          (end-marker (make-marker))
          (sort-fold-case t)
          matched
          )
      ;;Loop over all lines
      (goto-char (point-min))
      (set-marker start-marker (point))
      (while (re-search-forward get_num_re nil 't)
        (setq matched (match-string 1))
        (cond
         ((not last_num) t)
         ((not (string-equal last_num matched))
          (progn (set-marker end-marker (line-beginning-position))
                 (if (> (- end-marker 1) start-marker)
                     (sort-lines nil start-marker (- end-marker 1)))
                 (goto-char start-marker)
                 (insert "** ")
                 (goto-char end-marker)
                 (set-marker start-marker end-marker)
                 )))
        (setq last_num matched)
        (forward-line)
        )
      ;;clean up last group:
      (set-marker end-marker (line-beginning-position))
      (if (> end-marker start-marker)
          (sort-lines nil start-marker (- end-marker 1)))
      (goto-char start-marker)
      (insert "** ")
      ;;Add Header:
      (goto-char (point-min))
      (insert "* Tag Summary for: " source_name "\n")
      (indent-region (point-min) (point-max))
      )
    )
  )
(defun +jg-text-org-split-temp-buffer-create (args)
  "Given a pair, create a temp buffer in the cdr directory,
naming the directory based on the first line of text and insert the car "
  ;; (message "Creating Temp buffer for: %s" args)
  (assert (f-dir? (cdr args)))
  (with-temp-buffer
    (org-mode)
    (insert (car args))
    (goto-char (point-min))
    (re-search-forward "^\\*\\* ")
    (write-file (f-join (cdr args) (format "%s.org" (string-trim (buffer-substring (point) (line-end-position))))))
    )
  )
