;;; batch-run.el -*- lexical-binding: t; -*-
;;; https://orgmode.org/manual/HTML-Export.html
(require 'subr-x)
(require 'cl-lib)

(add-to-list 'load-path "/Volumes/documents/github/__configs/packages/ox-html-epub/")

(cl-loop for file in (directory-files "/Volumes/documents/github/_libs/lisp/doom_native/.local/straight/repos" t)
         if (file-directory-p file)
         do
         (add-to-list 'load-path file)
         )

(require 'ox-epub)

(defun org-epub-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension "html")))
	 (file (org-export-output-file-name extension))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'epub file
      async subtreep visible-only body-only ext-plist)))



;; (message "Load Path: %s" (string-join load-path "\n"))

;; emacs --batch example.org  --script /path/to/batch-run.el -f org-epub-export-to-html
