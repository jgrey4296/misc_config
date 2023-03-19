;;; +metadata.el -*- lexical-binding: t; -*-

(defun +jg-bibtex-meta-retrieval ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (files (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (result (cl-loop for file in files
                            collect
                            (shell-command-to-string (format "ebook-meta %s" (shell-quote-argument (expand-file-name (if (f-relative? file) (f-join jg-bibtex-pdf-loc file) file)))))
                            ))
           )
      (with-temp-buffer-window "*Metadata*" 'display-buffer-pop-up-window nil
        (princ (s-join "\n" result))
        )
      )
    )
  )

(defun +jg-bibtex-apply-meta ()
  " Use Calibre's ebook-meta program to take bibtex data and apply it to a pdf or epub "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (target (shell-quote-argument (bibtex-autokey-get-field "file")))
           (keys (mapcar #'car entry))
           (meta-opts '(("title     " . "-t")
                        ("author    " . "-a")
                        ("comments  " . "-c")
                        ("publisher " . "-p")
                        ("series    " . "-s")
                        ("number    " . "-i")
                        ("rating    " . "-r")
                        ("date      " . "-d")
                        ("isbn      " . "--isbn")
                        ("ident     " . "--identifier=")
                        ("tags      " . "--tags=")
                        ("category  " . "--category=")
                        ("*Apply*")
                        ))
          current
          )
      ;; read fields and wrap
      (while (not (s-equals? "*Apply*" (setq current (ivy-read "Assign to option: " meta-opts))))
        (when (alist-get current meta-opts nil nil #'equal)
          (push (cons  (alist-get current meta-opts nil nil #'equal)
                       (let* ((field (ivy-read "Select Field: " keys))
                              (text (bibtex-autokey-get-field field)))
                         (if (string-empty-p text) field text)))
                arg-pairs)
          )
        )
      (unless arg-pairs
        (error "Nothing to do"))
      ;; build and call
      (let* ((options (s-join " " (cl-loop for pair in arg-pairs
                                          collect
                                          (format (if (s-matches? "=$" (car pair)) "%s\"%s\"" "%s \"%s\"") (car pair) (cdr pair)))))
             (command (format "ebook-meta %s %s" target options))
             )
        (message "Command: %s" command)
        (shell-command command)
        )
      )
    )
  )

(defun +jg-bibtex-set-ebook-cover ()
  " Use Calibre's ebook-meta program to select an image and apply it as an epub's cover image "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (target (shell-quote-argument (bibtex-autokey-get-field "file")))

           )
      )
    )
  )
