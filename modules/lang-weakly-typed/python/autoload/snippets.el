;;; snippets.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-insert-import (&optional arg)
  " insert the literal string provided/read from minibuffer, at the imports section
of a python file "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((arg (if (not arg) (read-string "Import Statement: " "import ") arg)))
      (re-search-forward (code-shy-fold-block-gen :name "imports" :re t))
      (re-search-forward "^$")
      (insert "\n")
      (insert arg)
      )
    )
  )

;;;###autoload
(defun +jg-python-import-snippet (&optional arg)
  " Expand a yasnippet template, then insert it at the imports section "
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let* ((template-alist (mapcar (lambda (x) `(,(yas--template-uuid x) . ,x)) (yas--all-templates (yas--get-snippet-tables))))
         (template-name (ivy-completing-read "Import Snippet: " template-alist nil nil "import " ))
         (yas--current-template (alist-get template-name template-alist nil nil 'equal))
         final)
    (if yas--current-template
        (progn (with-temp-buffer
                 (yas-minor-mode)
                 (yas-expand-snippet yas--current-template (point-min))
                 (setq final (buffer-string))
                 )
               (pyimport--insert-import final)
               )
      )
    )
  )
