;;; editor/snippets/autoload/snippets.el -*- lexical-binding: t; -*-

(defun +snippets--remove-p (x y)
  (and (equal (yas--template-key x) (yas--template-key y))
       (file-in-directory-p (yas--template-get-file x) doom-emacs-dir)))

;;;###autoload
(defun +snippets-prompt-private (prompt choices &optional display-fn)
  "Prioritize private snippets over built-in ones if there are multiple
choices.

There are two groups of snippets in Doom Emacs. The built in ones (under
`doom-emacs-dir'; provided by Doom or its plugins) or your private snippets
(outside of `doom-eamcs-dir').

If there are multiple snippets with the same key in either camp (but not both),
you will be prompted to select one.

If there are conflicting keys across the two camps, the built-in ones are
ignored. This makes it easy to override built-in snippets with private ones."
  (when (eq this-command 'yas-expand)
    (let* ((gc-cons-threshold most-positive-fixnum)
           (choices (condition-case _
                        (cl-remove-duplicates choices :test #'+snippets--remove-p)
                      (wrong-type-argument choices))))
      (if (cdr choices)
          (cl-loop for fn in (cdr (memq '+snippets-prompt-private yas-prompt-functions))
                   if (funcall fn prompt choices display-fn)
                   return it)
        (car choices)))))

(defun +snippet--get-template-by-uuid (uuid &optional mode)
  "Look up the template by uuid in child-most to parent-most mode order.
Finds correctly active snippets from parent modes (based on Yas' logic)."
  (cl-loop with mode = (or mode major-mode)
           for active-mode in (yas--modes-to-activate mode)
           if (gethash active-mode yas--tables)
           if (gethash uuid (yas--table-uuidhash it))
           return it))

;;;###autoload
(defun +snippet--completing-read-uuid (prompt all-snippets &rest args)
  (plist-get
   (text-properties-at
    0 (apply #'completing-read prompt
             (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                          (hash-table-values yas--tables)
                                                                        (yas--get-snippet-tables)))

                      for txt = (format "%-25s%-30s%s"
                                        (yas--template-key tpl)
                                        (yas--template-name tpl)
                                        (abbreviate-file-name (yas--template-load-file tpl)))
                      collect
                      (progn
                        (set-text-properties 0 (length txt) `(uuid ,(yas--template-uuid tpl)
                                                                   path ,(yas--template-load-file tpl))
                                             txt)
                        txt))
             args))
   'uuid))

(defun +snippet--abort ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-current-buffer))

(defvar +snippet--current-snippet-uuid nil)

;;;###autoload
(defun +snippet--edit ()
  (interactive)
  (when +snippet--current-snippet-uuid
    (let ((buf (current-buffer)))
      (+snippets/edit +snippet--current-snippet-uuid)
      (kill-buffer buf))))
