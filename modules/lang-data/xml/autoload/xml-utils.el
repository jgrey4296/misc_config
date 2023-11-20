;;; +xml-utils.el -*- lexical-binding: t; -*-
;; TODO make async
;;-- repl

;;;###autoload
(defun +jg-xml-load-into-repl ()
  " Insert new content into an xml lint repl "
  (interactive)
  (cond ((process-live-p (get-buffer-process jg-xml-xmllint-shell-buffer-name))
         ;; live, so load
         (with-current-buffer jg-xml-xmllint-shell-buffer-name
           (+jg-text-clear-buffer))
         (comint-simple-send (get-buffer-process jg-xml-xmllint-shell-buffer-name)
                             (format "load %s" (current-buffer)))
         )
        ((buffer-live-p jg-xml-xmllint-shell-buffer-name)
         ;; exists, no process
         (kill-buffer jg-xml-xmllint-shell-buffer-name)
         (+eval/open-repl-other-window)
        )
        (t) ;; don't do anything, as repl will be opened after cond
        )
  (+eval/open-repl-other-window)
  )

;;;###autoload
(defun +xml/open-repl ()
  " Open an xml lint shell comint on the current buffer / file "
  (interactive)
  (when (get-buffer jg-xml-xmllint-shell-buffer-name)
    (with-current-buffer jg-xml-xmllint-shell-buffer-name
      (erase-buffer)
      )
    )
  (when (process-live-p (get-buffer-process jg-xml-xmllint-shell-buffer-name))
    (kill-process (get-buffer-process jg-xml-xmllint-shell-buffer-name))
    )

  (let* ((fname (buffer-file-name (current-buffer)))
         (is-html (f-ext? fname "html"))
         (default-directory (f-parent fname))
         (comint-buff (apply #'make-comint-in-buffer "xmllint"
                                             jg-xml-xmllint-shell-buffer-name
                                             jg-xml-repl-command
                                             nil
                                             (-filter #'identity
                                                      (list (when is-html "--html")
                                                            "--shell"
                                                            fname))))
          )
    (message "Fname: %s" (shell-quote-argument fname))
    (set-process-sentinel (get-buffer-process jg-xml-xmllint-shell-buffer-name)
                          (lambda (process state)
                            (message "Xmllint: %s" state)
                            (when (get-buffer jg-xml-xmllint-shell-buffer-name)
                              (kill-buffer jg-xml-xmllint-shell-buffer-name)
                              )
                            )
                          )
    (pop-to-buffer comint-buff)
    )
  )

;;-- end repl

;;;###autoload
(defun +jg-xml-format-buffer ()
  " Take the file contents, format it through xml lint,
and redisplay
 "
  (interactive)
  (let* ((current (current-buffer))
         (temp-file (make-temp-file "xmllint-temp" nil nil (buffer-string)))
         (result (shell-command-to-string (format jg-xml-format-command-string temp-file)))
         )
    (with-current-buffer current
      (erase-buffer)
      (insert result)
      )
    )
  )

;;;###autoload
(defun +jg-xml-run-xidel (query)
  " Run an xpath query on the current buffer / file
Storing and displaying results in a buffer
uses xidel, outputs as xml
  "
  (interactive "sXML Query: ")
  (let* ((current (current-buffer))
         (fname (buffer-file-name current))
         (default-directory (f-parent fname))
         (result (shell-command-to-string (format jg-xml-xidel-command-string
                                                  query
                                                  (shell-quote-argument  (f-filename fname)))))
         )
    (with-current-buffer (get-buffer-create jg-xml-xpath-results-buffer-name)
      (erase-buffer)
      (insert result)
      (nxml-mode)
      )
    (display-buffer  jg-xml-xpath-results-buffer-name)
    )
  )

;;;###autoload
(defun +jg-xml-dired-all-ext (files &rest exts)
  "Error if all files aren't of the specified extension"
  (unless  (--all? it
                   (cl-loop for file in files
                            collect (--any? (f-ext? file it) exts)))
    (error "Marked Files need to be %s's" exts)))

;;-- dired utils

;;;###autoload
(defun +jg-xml-dired-run-xidel (query)
  "Run an xpath query on marked files, saving the output to separate new files"
  (interactive "sXML Query: ")
  (let* ((marked (dired-get-marked-files))
         (target-dir (read-directory-name "Output Directory: "))
         (prefix (read-string "Prefix filename with: "))
         (results (cl-loop for fname in marked
                           do
                           (let ((result (shell-command-to-string (format jg-xml-xidel-command-string
                                                                          query
                                                                          (shell-quote-argument fname))))
                                 (new-fname (f-join target-dir (concat prefix (f-swap-ext (f-filename fname) "xml"))))
                                 )
                             (append-to-file result nil new-fname)
                             ;; result

                             )
                           ))
         )
    1
    ;; (with-current-buffer (get-buffer-create jg-xml-xpath-results-buffer-name)
    ;;   (erase-buffer)
    ;;   (mapc #'insert results)
    ;;   (nxml-mode)
    ;;   )
    ;; (display-buffer  jg-xml-xpath-results-buffer-name)
    ;; )
  )

  )

;;;###autoload
(defun +jg-xml-dired-elements ()
  "Print out the element mapping using xmlstarlet of all marked files combined"
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "xml" "html" "htm")
  (dired-do-shell-command jg-xml-elements-command-string nil (dired-get-marked-files))
  )

;;;###autoload
(defun +jg-xml-dired-select ()
  "Run an xmlstarlet query template on marked files "
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "xml")
  (let ((template (read-string "Query Template: "))
        )
    (dired-do-shell-command (format jg-xml-select-command-string template)
                            nil
                            (dired-get-marked-files))
    )
  )

;;;###autoload
(defun +jg-xml-dired-validate ()
  "Validate each marked file against an .xsd schema"
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "xml")
  (let ((schema (read-file-name "Schema File: "))
        (args '("-e"  "--net" "-s" "%s" "?" "2>>" "validation.errors"))
        (cmd jg-xml-validate-command-basic)
        )
    (dired-do-shell-command (format (concat cmd (s-join " "args)) schema)
                            nil
                            (dired-get-marked-files))
    )
  )

;;;###autoload
(defun +jg-xml-dired-format ()
  "Format xml files into fmt-`?` new files with:
an indent of 4,
recovering everything parsable,
removing redundant namespaces,
encoding as utf-8"
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "xml" "html" "htm")
  (let ((files (dired-get-marked-files t))
        (args '("-s" "4" "-R" "-N" "-e" "utf-8" "?" ">" "fmt-`?`"))
        (cmd jg-xml-format-command-basic)
        )
    (when (--any? (or (f-ext? it "html") (f-ext? it "htm"))  files)
      (push "--html" args))
    (message "Test Running on: %s : %s" default-directory files)
    (message "Command: %s" (concat cmd (s-join " " args)))
    (dired-do-shell-command (concat cmd (s-join " " args))
                            nil files)
    )
  )

;;;###autoload
(defun +jg-xml-dired-generate-schema ()
  "Generate a trang .xsd schema file from marked files "
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "xml")
  (dired-do-shell-command (format jg-xml-schema-command-string (read-string "Schema Name: "))
                          nil (dired-get-marked-files))
  )

;;;###autoload
(defun +jg-xml-dired-schema-uml ()
  "Convert an .xsd file into a plantuml .pu file, and create a png and text output for it "
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "xsd")
  (dired-do-shell-command jg-xml-schema-plantuml-command-string nil (dired-get-marked-files))
  (dired-do-shell-command "plantuml -filename schema.png"                 nil '("schema.pu"))
  (dired-do-shell-command "cat ? | plantuml -ttxt -p"                     nil '("schema.pu"))
  )

;;;###autoload
(defun +jg-xml-dired-gen-python ()
  "Generate python bindings for the marked files/directories"
  (interactive)
  (let ((package (read-string "Package Name: "))
        )
    (dired-do-shell-command (format jg-xml-python-generate-command-string package)
                            nil
                            (dired-get-marked-files))
    )
  )

;;;###autoload
(defun +jg-xml-dired-visualise-json ()
  "Use Plantuml to visualise json files"
  (interactive)
  (+jg-xml-dired-all-ext (dired-get-marked-files) "json")
  (dired-do-shell-command jg-xml-wrap-json-command-string nil (dired-get-marked-files))
  (dired-do-shell-command "open `?`.png" nil (dired-get-marked-files))
  )
;;-- end dired utils
