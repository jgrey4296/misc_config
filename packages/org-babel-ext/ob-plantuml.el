;;; ob-plantuml.el -*- lexical-binding: t; -*-

(defun jg-ob-plantuml-execute (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'plantuml-mode)
  (message "%s" params)
  (let* ((preview (assq :preview params))
         (body (replace-regexp-in-string
                "^[[:blank:]\n]*\\(@start\\)"
                "\\\\\\1"
                body))
         (fullbody (org-babel-plantuml-make-body body params))
         (out-file (cond ((assq :file params)
                          (cdr (assq :file params)))
                         (preview
                          (org-babel-temp-file "plantuml-" ".utxt"))
                         (t
                          (org-babel-temp-file "plantuml-" ".png"))))
         (in-file (org-babel-temp-file "plantuml-")))
    (cond ((eq plantuml-default-exec-mode 'server)
           (jg-ob-plantuml-server fullbody out-file))
          (preview
           (jg-ob-plantuml-preview fullbody params in-file out-file))
          (t
           (jg-ob-plantuml-render fullbody params in-file out-file)))))

(defun jg-ob-plantuml-server (body out-file)
  (message "WARNING: USING PLANTUML SERVER")
  (if (bound-and-true-p org-export-current-backend)
      (user-error "Exporting plantuml diagrams in server mode is not supported (see `plantuml-default-exec-mode')")
    (save-current-buffer
      (save-match-data
        (with-current-buffer
            (url-retrieve-synchronously (plantuml-server-encode-url body)
                                        nil t)
          (goto-char (point-min))
          ;; skip the HTTP headers
          (while (not (looking-at "\n")) (forward-line))
          (delete-region (point-min) (+ 1 (point)))
          (write-file out-file)))))
  ;; Return filename
  out-file
  )

(defun jg-ob-plantuml-render (body params in-file out-file)
  (let* ((cmd (concat (cond ((eq plantuml-default-exec-mode 'executable)
                             (unless (executable-find plantuml-executable-path)
                               (error "Could not find plantuml at %s"
                                      (executable-find plantuml-executable-path)))
                             (concat (shell-quote-argument (executable-find plantuml-executable-path))
                                     " --headless"))
                            ((not (file-exists-p plantuml-jar-path))
                             (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
                            ((concat "java " (cdr (assoc :java params)) " -jar "
                                     (shell-quote-argument
                                      (expand-file-name plantuml-jar-path)))))
                      " "
                      (pcase (file-name-extension out-file)
                        ("png" "-tpng")
                        ("svg" "-tsvg")
                        ("eps" "-teps")
                        ("pdf" "-tpdf")
                        ("tex" "-tlatex")
                        ("vdx" "-tvdx")
                        ("xmi" "-txmi")
                        ("scxml" "-tscxml")
                        ("html" "-thtml")
                        ("txt" "-ttxt")
                        ("utxt" "-utxt"))
                      " "
                      " -p " (cdr (assoc :cmdline params)) " < "
                      (org-babel-process-file-name in-file)
                      " > "
                      (org-babel-process-file-name out-file))))
    (with-temp-file in-file (insert body))
    (message "%s" cmd)
    (org-babel-eval cmd ""))
  out-file
  )

(defun jg-ob-plantuml-preview (body params in-file out-file)
  (with-temp-buffer
    (insert-file-contents (jg-ob-plantuml-render body params in-file out-file))
    (buffer-string))
  )
