;;; +advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-mail-header-summary ()
  "Return a message summary based on the message headers.
The value is a list of two strings, the first and second parts of the summary.

The current buffer must already be narrowed to the message headers for
the message being processed."
  (goto-char (point-min))
  (let ((date (save-excursion
	        (if (not (re-search-forward "^Date:" nil t))
		    "??????"
	          ;; Match month names case-insensitively
	          (cond ((let ((case-fold-search t))
			   (re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
					      (line-end-position) t))
		         (format "%2d-%3s"
			         (string-to-number (buffer-substring
						    (match-beginning 2)
						    (match-end 2)))
			         (buffer-substring
			          (match-beginning 4) (match-end 4))))
		        ((let ((case-fold-search t))
			   (re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
					      (line-end-position) t))
		         (format "%2d-%3s"
			         (string-to-number (buffer-substring
						    (match-beginning 4)
						    (match-end 4)))
			         (buffer-substring
			          (match-beginning 2) (match-end 2))))
		        ((re-search-forward "\\(19\\|20\\)\\([0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)"
		                            (line-end-position) t)
		         (format "%2s%2s%2s"
			         (buffer-substring
			          (match-beginning 2) (match-end 2))
			         (buffer-substring
			          (match-beginning 3) (match-end 3))
			         (buffer-substring
			          (match-beginning 4) (match-end 4))))
		        (t "??????")))))
        (from (save-excursion
	         (and (re-search-forward "^From:[ \t]*" nil t)
		      (mail-strip-quoted-names
		       (buffer-substring
			(1- (point))
                        (line-end-position))))))
        (subject (save-excursion
                   (if (re-search-forward "^Subject:" nil t)
	               (let (pos str)
		         (skip-chars-forward " \t")
		         (setq pos (point))
		         (forward-line 1)
		         (setq str (buffer-substring pos (1- (point))))
		         (while (looking-at "[ \t]")
		           (setq str (concat str " "
				             (buffer-substring (match-end 0)
						               (line-end-position))))
		           (forward-line 1))
		         str)
	             (re-search-forward "[\n][\n]+" nil t)
	             (buffer-substring (point) (line-end-position)))))
        )
    (list (replace-regexp-in-string "\n+" " "
                                    (concat date " | "
                                            (substring from 0 (s-index-of "@" from)) " | "
                                            (propertize "Lines: "
                                                        'font-lock-ignore t
                                                        'face '(:foreground "green"))))
          ;; TODO add custom highlight list for subject
          (concat "| Subject: " (replace-regexp-in-string "\n+" " " subject)
                  "\n"))
    )
  )

;;;###autoload
(defun +jg-mail-create-summary (msgnum deleted unseen lines)
  (goto-char (point-min))
  (let ((line (rmail-header-summary))
	(labels (rmail-get-summary-labels))
        status prefix basic-start basic-end linecount-string
        result)

    (setq linecount-string
	  (cond
	   ((not lines)       " ")
	   ((<= lines      9) (format "   [%d]" lines))
	   ((<= lines     99) (format "  [%d]" lines))
	   ((<= lines    999) (format " [%d]" lines))
	   ((<= lines   9999) (format "  [%dk]" (/ lines 1000)))
	   ((<= lines  99999) (format " [%dk]" (/ lines 1000)))
	   (t                 (format "[%dk]" (/ lines 1000)))))

    (setq status (cond
		  (deleted ?D)
		  (unseen ?-)
		  (t ? ))
	  prefix (format "%5d%c " msgnum status)
	  basic-start (car line)
	  basic-end (cadr line))
    ;; (funcall rmail-summary-line-decoder
    (setq result (concat prefix basic-start linecount-string " "
                         labels basic-end))
    result
    )
  )

;;;###autoload
(defun +jg-mail-new-summary-princ-override (fn &rest rst)
  (if (function-get 'princ 'original)
      (unwind-protect
          (progn (fset 'princ (function-get 'princ 'mod))
                 (apply fn rst))
        (fset 'princ (function-get 'princ 'original)))
    (apply fn rst))
  )

;;;###autoload
(defun +jg-mail-summary-update-princ-override (fn &rest rst)
  (if (function-get 'princ 'original)
      (unwind-protect
          (progn (fset 'princ (function-get 'princ 'mod))
                 (apply fn rst))
        (fset 'princ (function-get 'princ 'original)))
    (apply fn rst))
  )

(defun +jg-mail-princ-as-insert (x buf)
  " For locally overriding use of princ
when mail builds the mail-summary buffer,
as princ strips out any text properties"
  (with-current-buffer buf
    (insert x)
    ))

(function-put 'princ 'original (symbol-function 'princ))
(function-put 'princ 'mod (symbol-function '+jg-mail-princ-as-insert))

;;;###autoload
(advice-add 'rmail-header-summary :override #'+jg-mail-header-summary)

;;;###autoload
(advice-add 'rmail-create-summary :override #'+jg-mail-create-summary)

;;;###autoload
(advice-add 'rmail-new-summary-1 :around #'+jg-mail-new-summary-princ-override)

;;;###autoload
(advice-add 'rmail-summary-update-line :around #'+jg-mail-summary-update-princ-override)
