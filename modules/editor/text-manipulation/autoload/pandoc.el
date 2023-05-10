;;; +pandoc.el -*- lexical-binding: t; -*-

(defvar jg-text-pandoc-cmd "pandoc")
(defvar jg-text-pandoc-args '())
(defvar jg-text-pandoc-style-fn "pandoc.style")

(defun +jg-text-pandoc-gen-style ()
  (interactive)
  (with-temp-file (f-join default-directory jg-text-pandoc-style-fn)
    (call-process jg-text-pandoc-cmd nil t nil "--print-highlight-style" "pygments")
    )
  )

(defun +jg-text-pandoc-compile ()
  (interactive)
  (apply #'call-process jg-text-pandoc-cmd nil nil nil (append jg-text-pandoc-args
                                                               (dired-get-marked-files)
                                                               (list "-o" (read-file-name "Output: " default-directory "out.pdf"))
                                                               )
         )

  )
