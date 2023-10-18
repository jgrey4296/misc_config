;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-snippets-complete-or-snippet (&optional arg)
  (interactive "p")
  (cond ((company-complete-common-or-cycle) nil)
        ((yas-expand-from-trigger-key) nil)
        (t (indent-for-tab-command))
      )
    )

;;;###autoload
(defun +jg-snippets-new-snippet()
  "Create a new snippet in `+snippets-dir'."
  (interactive)
  (let ((default-directory
          (expand-file-name (symbol-name major-mode)
                            +snippets-dir)))
    (+jg-snippets--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (+file-templates--expand t :mode 'snippet-mode)
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

;;;###autoload
(defun +jg-snippets--ensure-dir (dir)
  (unless (file-directory-p dir)
    (if (y-or-n-p (format "%S doesn't exist. Create it?" (abbreviate-file-name dir)))
        (make-directory dir t)
      (error "%S doesn't exist" (abbreviate-file-name dir)))))

;;;###autoload
(defun +jg-snippets-yas-prompt-fn (prompt choices &optional display-fn)
  " Yasnippet ivy which shows groups "
  (let* ((max-name 0)
         (max-group 0)
         (index 0)
         ;; Get the bits i care about to create the display
         (choice-parts (cl-loop for template in choices
                                collect
                                (let ((name (s-replace "\\" "/" (s-trim (yas--template-name template))))
                                      (group (s-trim (apply 'concat (yas--template-group template))))
                                      (table (concat "Table:" (yas--table-name (yas--template-table template))))
                                      )
                                  (cl-incf index)
                                  (setq max-name (max (length name) max-name)
                                        max-group (max (length group) max-group))
                                  (list (list name (length name)) (list group (length group)) table (1- index)))))
         (format-fn (lambda (x) `(,(concat " "
                                           ;; template name + gap
                                           (caar x) (make-string (- (+ 5 max-name) (cadar x)) ? ) " : "
                                           ;; groups
                                           (caadr x) (make-string (- (+ 5 max-group) (cadadr x)) ? ) " : "
                                           ;; table
                                           (caddr x))
                                  ;; index
                                  . ,(car (last x)))))
         chosen)
    ;; only once i know the max-name, format the choices
    ;; use :action rather than return value, to use the (str . index) pair
    (ivy-read "Choose Snippet: " (mapcar format-fn choice-parts) :caller '+jg-snippets-yas-ivy :action (lambda (x) (setq chosen (cdr x))))
    (nth (or chosen 0) choices)
    )
  )

;;;###autoload
(defun +jg-snippets-debug-dirs ()
  (interactive)
  (message (string-join yas-snippet-dirs "\n"))
  )
