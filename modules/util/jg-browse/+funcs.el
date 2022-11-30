;;; +browsing.el -*- lexical-binding: t; -*-

;;-- interactive
(defun +jg-browse-url (&optional url)
  " quick acess to search handlers "
  (interactive)
  (let ((url (cond (url url)
                   ((eq evil-state 'visual)
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t nil)))
        )
    (cond ((not url)
           (+lookup/online-select))
          ((f-exists? url)
           (shell-command (format "open %s" url)))
          (t
           (browse-url url)
           )
          )
    )
  )

(defun +jg-browse-toggle-browsing ()
  (interactive)
  (let* ((index (1+ (or (-elem-index jg-browse-selected-prog jg-browse-variant-progs) 0)))
        (new-prog (nth (mod index (length jg-browse-variant-progs)) jg-browse-variant-progs))
        )
    (message "Browsing: %s" (setq jg-browse-selected-prog new-prog))
    )
  )

(defun +jg-browse-toggle-preview ()
  (interactive)
  (message "Using Preview for pdfs: %s"
           (setq jg-browse-use-preview (not jg-browse-use-preview)))
  )

(defun +jg-browse-reapply-providers ()
  (interactive)
  (setq +lookup-provider-url-alist jg-browse-providers-alist)
  )

;;-- end interactive

;;-- browse program selection
(defun +jg-browse-default (url &rest args)
  "
Find and call the appropriate browser program,
after `browse-url-handlers` have processed the url
"
  (cond ((-contains? args 'quicklook)
         (call-process "qlmanage" nil nil nil "-p" (shell-quote-argument url)))
        ((and (-contains? args 'local) (f-ext? url "epub"))
         (apply 'call-process "open" nil nil nil url jg-browse-epub-args))
         ((and (-contains? args 'local) (f-ext? url "pdf") jg-browse-use-preview)
          (apply 'call-process "open" nil nil nil url jg-browse-pdf-args))
         ((not (s-equals? jg-browse-selected-prog "eww"))
          (message "Using %s" jg-browse-selected-prog)
          (call-process "open" nil nil nil "-a" jg-browse-selected-prog url))
         (t
          (eww-browse-url url args))
         )
  )

;;-- end browse program selection

;;-- browse handlers
(defun +jg-browse-twitter (url &rest args)
  (+jg-browse-default (format "%s/%s" jg-browse-twitter-url (substring url 1)))
  )

(defun +jg-browse-amazon (url &rest args)
  ;; Handle US and UK
  )

;;-- end browse handlers
