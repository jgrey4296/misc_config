;;; +nav.el -*- lexical-binding: t; -*-
;; Customisations of Conda navigation

;;;###autoload
(defun +jg-conda-pop-to-xref (result)
  (if (stringp result)
      (message result)
    (let* ((carousel-suppress-adding t)
           (xrefs (anaconda-mode-make-xrefs result))
           (marker (save-excursion (xref-location-marker (xref-item-location (cl-first xrefs)))))
           (buf (marker-buffer marker))
           )
      (+popup-buffer buf)
      (with-current-buffer buf
        (xref--goto-char marker))
      )
    )
  )

;;;###autoload
(defun +jg-conda-find-defs ()
  (interactive)
  (anaconda-mode-call "infer" #'+jg-conda-pop-to-xref)
  )

;;;###autoload
(defun +jg-conda-show-doc ()
  (interactive)
  (anaconda-mode-call "show_doc" #'+jg-conda-show-doc-callback)
  )

;;;###autoload
(defun +jg-conda-show-doc-callback (result)
  (if (> (length result) 0)
      (+popup-buffer (anaconda-mode-documentation-view result))
    (message "No documentation available"))
  )

;;;###autoload
(defun +jg-conda-find-assignments ()
  (interactive)
  (anaconda-mode-call
   "goto"
   #'(lambda (result)
     (anaconda-mode-show-xrefs result nil "No assignments found")))
  )

;;;###autoload
(defun +jg-conda-find-references ()
  (interactive)
  (anaconda-mode-call
   "get_references"
   #'(lambda (result)
     (anaconda-mode-show-xrefs result nil "No references found")))
)

;;;###autoload
(defun +jg-conda-eldoc ()
  (interactive)
  (anaconda-mode-call
   "eldoc"
   #'anaconda-mode-eldoc-callback)
  )
