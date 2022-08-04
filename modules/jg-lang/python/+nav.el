;;; +nav.el -*- lexical-binding: t; -*-

;; Customisations of navigation functions

(evil-define-motion +jg-python-forward-defun (count)
  " Custom Python movement, taking fold-blocks into account "
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (let ((fold-block-pos (point))
        (defun-pos (point)))
    (save-excursion
      (setq defun-pos (python-nav-forward-defun)))
    (save-excursion
      (setq fold-block-pos (re-search-forward (+jg-text-fold-block-gen :re t) defun-pos t)))
    (goto-char (apply 'min (mapcar #'(lambda (x) (if x x (point-max))) (list fold-block-pos defun-pos))))
    )
)



;; Customisations of Conda navigation
;; TODO add code for using window-ring

(defun +jg-conda-find-defs ()
  (interactive)
  (anaconda-mode-call "infer"
                      #'(lambda (result)
                        (message "%s" result)
                        (anaconda-mode-show-xrefs result 'window "None Found")))
  )

(defun +jg-conda-show-doc ()
  (interactive)
  (anaconda-mode-call "show_doc" '+jg-conda-show-doc-callback)
  )

(defun +jg-conda-show-doc-callback (result)
  (if (> (length result) 0)
      (+popup-buffer (anaconda-mode-documentation-view result))
    (message "No documentation available"))
  )

(defun +jg-conda-find-assignments ()
  (interactive)
  (anaconda-mode-call
   "goto"
   #'(lambda (result)
     (anaconda-mode-show-xrefs result nil "No assignments found")))
  )

(defun +jg-conda-find-references ()
  (interactive)
  (anaconda-mode-call
   "get_references"
   #'(lambda (result)
     (anaconda-mode-show-xrefs result nil "No references found")))
)

(defun +jg-conda-eldoc ()
  (interactive)
  (anaconda-mode-call
   "eldoc"
   'anaconda-mode-eldoc-callback)
  )
