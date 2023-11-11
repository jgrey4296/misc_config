;;; +metadata.el -*- lexical-binding: t; -*-
(require 'bibtex)
(require 'org-ref-bibtex)

(defconst jg-bibtex-meta-buffer "*Metadata*")

(defconst jg-bibtex-meta-program "ebook-meta")

(defconst jg-bibtex-meta-opts '(("title     " . "-t")
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
                                ("*Apply*")))

;;;###autoload
(defun +jg-bibtex-meta-retrieval ()
  " Use 'jg-bibtex-meta-program to retrieve metadata about files in current bibtex entry "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (files (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (procs (cl-loop for file in files
                            collect
                            (start-process
                             (format "bib:meta:%s" (f-base file))
                             (format "*bib:meta:%s*" (f-base file))
                             "ebook-meta"
                             (expand-file-name
                              (if (f-relative? file) (f-join jg-bibtex-pdf-loc file) file))
                             )
                            )
                   )
           )
      (with-process-wrap! jg-bibtex-meta-buffer procs)
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-apply-meta ()
  " Use Calibre's ebook-meta program to take bibtex data and apply it to a pdf or epub "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (targets (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (keys (mapcar #'car entry))
           (meta-opts jg-bibtex-meta-opts)
          current
          options
          )
      (unless targets
        (error "No files to apply to"))
      ;; select fields and wrap
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
        (error "Nothing selected to apply"))

      ;; Convert ready for use as args
      (setq options (cl-loop for pair in arg-pairs
                             append (list (car pair) (cdr pair))))
                             ;; (format (if (s-matches? "=$" (car pair))
                             ;;             "%s\"%s\"" "%s \"%s\"")
                             ;;         (car pair) (cdr pair))
                             ;; )

      (ivy-read "Apply Metadata to: "
                (mapcar #'(lambda (x) (cons (f-base x) x)) targets)
                :multi-action (-partial #'+jg-bibtex-apply-meta-fn options)
                :action (-partial #'+jg-bibtex-apply-meta-solo-fn options)
                )

      )
    )
  )

(defun +jg-bibtex-apply-meta-solo-fn (args file)
  (+jg-bibtex-apply-meta-fn args (list file))
  )

(defun +jg-bibtex-apply-meta-fn (args files)
  (with-process-wrap! jg-bibtex-meta-buffer
                      (cl-loop for file in files
                               collect
                               (apply #'start-process
                                      (format "meta:apply:%s" (f-base (cdr file)))
                                      (format "*meta:apply:%s*" (f-base (cdr file)))
                                      jg-bibtex-meta-program
                                      (cdr file)
                                      args
                                      )
                               )
                      )
  (with-current-buffer jg-bibtex-meta-buffer
    (insert (format "Process Args: %s\n" args))
    )
  )

;;;###autoload
(defun +jg-bibtex-set-ebook-cover ()
  " Use Calibre's ebook-meta program to select an image and apply it as an epub's cover image "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (ebook nil)
           (cover nil)
           (target (start-process
                    ("meta:cover:%s"
                    jg-bibtex-meta-program
                    (bibtex-autokey-get-field "file"))))

           )
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-update-entry ()
  (interactive)
  (when (org-ref-bibtex-entry-doi)
    (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi))
    )
  )

;;;###autoload
(defun +jg-bibtex-insert-entry-from-doi ()
  (interactive)
  (let* ((doi (read-string "Doi: "))
         (results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type))
         (bibtex (-some (lambda (g) (funcall g type results)) doi-utils-bibtex-type-generators))
         )
    (cond ((not bibtex)
           (insert "@misc{,\n")
           (while results
             (let ((key (string-replace ":" "" (symbol-name (pop results))))
                   (value (pop results)))
               (if (not (vectorp value))
                   (insert (format "%s = {%s},\n" key value))
                 (dolist (x (append value nil))
                   (insert (format "%s = {%s},\n" key (-remove #'keywordp x)))
                   )
                 )
               )
             )
           (insert "\n}")
           )
          (t
           (insert bibtex)
           (backward-char)
           ;; set date added for the record
           (when-let (ts (funcall doi-utils-timestamp-format-function)) (bibtex-set-field doi-utils-timestamp-field ts))
           (org-ref-clean-bibtex-entry)
           (save-buffer))
          )
    )
  )
