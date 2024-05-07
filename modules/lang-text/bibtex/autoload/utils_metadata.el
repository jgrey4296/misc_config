;;; +metadata.el -*- lexical-binding: t; -*-
(require 'bibtex)
(require 'org-ref-bibtex)

(defconst jg-bibtex-meta-buffer "*Metadata*")

(defconst jg-bibtex-meta-program "ebook-meta")

(defconst jg-bibtex-full-meta-program "exiftool")

(defconst jg-bibtex-full-meta-args (list
                                    "-g"                    ;; print group headings
                                    "-a"                    ;; allow duplicates
                                    "-u"                    ;; extract unknown tags
                                    ;; "-s"       ;; short tag names
                                    ;; "-n"       ;; no print conversion
                                    ;; "-X"       ;; xml format
                                    ;; "-j" ;; json format
                                    "-pdf:all"              ;; all pdf tags
                                    "-XMP:all"              ;; all xmp tags
                                    "-XML:all"              ;; all xml (epub) tags
                                    "-zip:all"
                                    "-file:filepath"
                                    "-file:filemodifydate"
                                    "-file:filetype"
                                    "-file:filesize"

                                    "--xml:manifest*"       ;; exclude manifest tags
                                    "--xml:spine*"          ;; exclude spine tags
                                    "--xml:guidereference*" ;; exclude guideref tags
                                    ))

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
                           (apply #'start-process
                                  (format "bib:meta:%s" (f-base file))
                                  (format "*bib:meta:%s*" (f-base file))
                                  jg-bibtex-full-meta-program
                                  (-concat jg-bibtex-full-meta-args
                                           (list (expand-file-name
                                                  (if (f-relative? file)
                                                      (f-join jg-bibtex-pdf-loc file)
                                                    file))
                                                 )
                                           )
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
  "update a bibtex entry by retrieving it's doi information"
  (interactive)
  (when (org-ref-bibtex-entry-doi)
    (+jg-bibtex-doi-update (org-ref-bibtex-entry-doi))
    )
  )

;;;###autoload
(defun +jg-bibtex-insert-entry-from-doi ()
  "given a doi, create an entry for it"
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

(defun +jg-bibtex-doi-update (doi)
  "Update fields in a bibtex entry from the DOI.
Every field will be updated, so previous change will be lost."
  (interactive (list (or (replace-regexp-in-string "https?://\\(dx.\\)?doi.org/" "" (bibtex-autokey-get-field "doi"))
                         (read-string "DOI: "))))
  (let* ((results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type))
         (author (mapconcat (lambda (x) (or (plist-get x :name) (concat (plist-get x :family) ", " (plist-get x :given)))) (plist-get results :author) " and "))
         (editor (mapconcat (lambda (x) (or (plist-get x :name) (concat (plist-get x :family) ", " (plist-get x :given)))) (plist-get results :editor) " and "))
         (title (plist-get results :title))
         (subtitle (if (seq-empty-p (plist-get results :subtitle)) "" (elt (plist-get results :subtitle) 0)))
         (journal (if (seq-empty-p (plist-get results :container-title)) "" (plist-get results :container-title)))
         (year (format "%s" (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 0)))
         (volume (plist-get results :volume))
         (number (or (plist-get results :issue) ""))
         (pages (or (plist-get results :page) ""))
         (url (or (plist-get results :URL) ""))
         (doi (plist-get results :DOI))
         (isbn (elt (or (plist-get results :ISBN) [""]) 0))
         (pub (plist-get results :publisher))
         mapping)

    ;; map the json fields to bibtex fields. The code each field is mapped to is
    ;; evaluated.
    (setq mapping `((:author          . (bibtex-set-field "author" ,author))
                    (:editor          . (bibtex-set-field "editor" ,editor))
                    (:title           . (bibtex-set-field "title" ,title))
                    (:subtitle        . (bibtex-set-field "subtitle" ,subtitle))
                    (:container-title . (bibtex-set-field "journal" ,journal))
                    (:issued          . (bibtex-set-field "year" ,year))
                    (:volume          . (bibtex-set-field "volume" ,volume))
                    (:issue           . (bibtex-set-field "number" ,number))
                    (:page            . (bibtex-set-field "pages" ,pages))
                    (:DOI             . (bibtex-set-field "doi" ,doi))
                    (:URL             . (bibtex-set-field "url" ,url))
                    (:ISBN            . (bibtex-set-field "isbn" ,isbn))
                    (:publisher       . (bibtex-set-field "publisher" ,pub))
                    )
          )
    ;; now we have code to run for each entry. we map over them and evaluate the code
    (mapc (lambda (key) (eval (cdr (assoc key mapping))))
          (plist-get-keys results))

    (org-ref-clean-bibtex-entry)
    (when jg-bibtex-popup-doi-info
      (with-temp-buffer-window "*DOI Metadata*" nil nil
        (mapc (lambda (key) (princ (format "%s : %s\n" key (plist-get results key))))
              (plist-get-keys results))
        )
      )
    )
  )
