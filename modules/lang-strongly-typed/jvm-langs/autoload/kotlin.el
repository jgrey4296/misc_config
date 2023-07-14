;;; lang/kotlin/autoload.el -*- lexical-binding: t; -*-

;;;autoload
(defun +kotlin-locate-gradlew-file ()
  "Gradlew file location for this project."
  (locate-dominating-file buffer-file-name "gradlew"))

;;;###autoload
(defun +kotlin/run-gradlew (command)
  "Run gradlew in this project."
  (interactive "sCommand: ")
  (let ((default-directory (+kotlin-locate-gradlew-file))
        (compilation-read-command nil)
        (compile-command (format "sh gradlew %s" command)))
    (call-interactively #'compile)))

;;;###autoload
(defun +jg-kotlin-related-files-fn (path)
    " Given a relative path to a file, provide projectile with various :kinds of related file "
    (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
          (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
          ;;(init-file  (f-join (f-parent path) "__init__.py"))
          (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
          ;;(error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
          (project    (f-join (projectile-project-root) "project-file"))
          (is-test (s-matches? "^test_" (f-filename path)))
          )
      (append (when is-test (list :impl impl-file))
              (unless is-test (list :test test-file))
              (when (s-matches? "\/cli\/" path) (list :project project))
              (list :init-py init-file)
              (list :log log-file)
              (list :errors error-file)
              )
      )
    )

;;;###autoload
(defun +kotlin-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'kotlin-mode)
  (if (not (bufferp kotlin-repl-buffer))
      (kotlin-repl))
  (get-buffer-create kotlin-repl-buffer)
  )
