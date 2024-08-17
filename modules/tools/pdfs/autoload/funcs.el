;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org--pdftools-link-handler (fn &rest args)
      "Produces a link handler for org-pdftools that suppresses missing-epdfinfo errors whenever storing or exporting links."
      ;; hack Fixes an issue where org-pdftools link handlers will throw a
      ;;      'pdf-info-epdfinfo-program is not executable' error whenever any
      ;;      link is stored or exported (whether or not they're a pdf link). This
      ;;      error gimps org until `pdf-tools-install' is run, but this is poor
      ;;      UX, so we suppress it.
      (lambda (&rest args)
        (and (ignore-errors (require 'org-pdftools nil t))
             (file-executable-p pdf-info-epdfinfo-program)
             (apply fn args))))


(defvar jg-pdf-to-text-call "pdftotext")
(defvar jg-pdf-validate-call "qpdf")
(defvar jg-pdf-validate-args '("--check"))
(defvar jg-pdf-metadata-call "exiftool")
(defconst jg-pdf-metadata-args (list
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


;;;###autoload
(defun +jg-pdf-to-text ()
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             do
             (let* ((pages (pcase (read-string "Pages: ")
                             ((and x (pred (s-contains? "-" x)))
                              (s-split "-" x))
                             (x (list x x))
                             ))
                    (call (list jg-pdf-to-text-call
                                "-f" (car pages)
                                "-l" (cadr pages)
                                file
                                (f-swap-ext file "txt")
                                )
                          )
                    )
               (make-process :name (format "pdftotext-%s" (f-base file))
                             :buffer nil
                             :command call)

               )
             )
    )
  )

;;;###autoload
(defun +jg-pdf-linearize ()
  "run qpdf linearization"
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             do
             (let* ((call (list "qpdf"
                                file
                                "--linearize"
                                (f-join (f-dirname file)
                                        (format "%s-linear.pdf" (f-base file))
                                        )
                                )
                          )
                    )
               (make-process :name (format "qpdf linearization: %s" (f-base file))
                             :buffer nil
                             :command call)
               )
             )
    )
  )

;;;###autoload
(defun +jg-pdf-validate ()
  "Validate a pdf using qpdf "
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             if (f-ext? file "pdf")
             do
             (make-process :name (format "pdf-validate-%s" (f-base file))
                           :buffer (get-buffer-create (format "pdf-validate-%s" (f-base file)))
                           :command (append (list jg-pdf-validate-call)
                                            jg-pdf-validate-args
                                            (list file)
                                            )
                           ;; TODO :sentinel open buffer
                           )
             )
    )
  )

;;;###autoload
(defun +jg-pdf-metadata ()
  "Get pdf metadata using exiftool"
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             if (or (f-ext? file "pdf") (f-ext? file "epub"))
             do
             (make-process :name (format "pdf-metadata-%s" (f-base file))
                           :buffer (get-buffer-create (format "pdf-metadata-%s" (f-base file)))
                           :command (append (list jg-pdf-metadata-call)
                                            jg-pdf-metadata-args
                                            (list file)
                                            )
                           )
             )
    )
  )

;;;###autoload
(defun +jg-pdf-decrypt ()
  "remove owner restrictions on pdfs"
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             if (f-ext? file "pdf")
             do
             (message "Creating: %s" (f-join (f-parent file) (format "%s-unlocked.pdf" (f-base file))))
             (make-process :name (format "pdf-decrypt-%s" (f-base file))
                           :buffer nil
                           :command (list "qpdf" "--decrypt" file (f-join (f-parent file)
                                                                          (format "%s-unlocked.pdf" (f-base file))))
                           )
             )
    )

  )
