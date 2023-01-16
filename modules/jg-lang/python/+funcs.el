;;; lang/jg-python/+funcs.el -*- lexical-binding: t; -*-


;;-- display control
(defun +jg-python-close-all-defs ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (python-nav-forward-defun)
        (outline-hide-subtree)
        )
      )
    )
(defun +jg-python-close-class-defs ()
    (interactive)
    (save-excursion
      (end-of-line)
      (unless (not (re-search-backward "^class " nil t))
        (if (not current-prefix-arg)
            (progn
              (outline-hide-subtree)
              (outline-toggle-children))
          (outline-show-subtree)
          (forward-line)
          (while (and (python-nav-forward-defun)
                      (progn (beginning-of-line)
                             (not (looking-at-p "^class"))))
            (outline-toggle-children)
            (forward-line)
            )
          )
        )
      )
    )

;;-- end display control

;;-- insert commands
(defun +jg-python-insert-import (&optional arg)
  " insert the literal string provided/read from minibuffer, at the imports section
of a python file "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((arg (if (not arg) (read-string "Import Statement: " "import ") arg)))
      (re-search-forward (autohide-minor-mode-fold-block-gen :name "imports" :re t))
      (re-search-forward "^$")
      (insert "\n")
      (insert arg)
      )
    )
  )
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

;;-- end insert commands

;;-- selection control
;; TODO make these text objects?
(defun +jg-python-select-defun ()
  (interactive)
  (let ((start (progn (python-nav-beginning-of-defun)
                      (point)))
        (end (progn (python-nav-end-of-defun)
                    (point))))
    (evil-visual-make-region start end))
  )
(defun +jg-python-select-class ()
  (interactive)
  (let ((start (re-search-backward "^class")))
    (python-nav-end-of-defun)
    (evil-visual-make-region start (point))
    )
  )
;;-- end selection control

;;-- cleanup
(defun +jg-python-cleanup-import-blocks ()
  " Collect all ##-- imports blocks,
and move them to the start of the file,
then sort them
TODO
  "
  (interactive)
  (let ((source (current-buffer))
        (collected-imports (make-temp-file "collected-imports"))
        start end groupname cleaned)
    ;; Go from bottom of buffer to top
    (with-current-buffer source
      (goto-char (point-max))
      (while (re-search-backward (autohide-minor-mode-fold-block-gen :re t) nil t)
        (setq groupname (match-string 1))
        (cond ((and (s-matches? "^imports" groupname)
                    end
                    (not start))
               (beginning-of-line)
               (setq start (point)))
              ((and (s-matches? "^end imports" groupname)
                    (not end))
               (end-of-line)
               (setq end (point))))
        ;; Copy the block to the temp buffer
        (if (and start end)
            (progn
              (-if-let (folds (vimish-fold--folds-in start end))
                  (vimish-fold--delete (car folds)))
              (goto-char end)
              (insert "\n")
              (write-region start (+ 1 end) collected-imports t)
              (kill-region start end)
              (setq start nil
                    end nil)
              (end-of-line -0)
            )
          (end-of-line -0)
          )
        )
      )
    ;; Then cleanup the collect imports
    (with-temp-buffer
      (insert-file-contents collected-imports)
      (goto-char (point-min))
      (flush-lines "##-- ")
      (write-file collected-imports)
      (py-isort-buffer)
      (setq cleaned (s-trim (buffer-string)))
      (write-file collected-imports)
      )
    ;; And Insert back into original buffer
    (with-current-buffer source
      (goto-char (point-min))
      (re-search-forward "^\"\"\"" nil t)
      (re-search-forward "^\"\"\"" nil t)
      (end-of-line)
      (insert "\n")
      (insert (autohide-minor-mode-fold-block-gen :name "imports" :newlines t))
      (insert cleaned)
      (insert (autohide-minor-mode-fold-block-gen :name "imports" :newlines t :end t))
      )
    )
  )

(defun +jg-python-cleanup-whitespace ()
  (while (re-search-forward "\n\n\\(\n+\\)" nil t)
    (let ((matched (length (match-string 1))))
      (backward-delete-char matched)
      )
    )
  )

(defun +jg-python-cleanup-ensure-newline-before-def ()
  (while (re-search-forward "^\s+def " nil t)
    (forward-line -1)
    (end-of-line)
    (insert "\n")
    (forward-line 2)
    )
  )
;;-- end cleanup

;;-- summary
(defun +jg-python-class-diagram ()
  " On lines of class definitions 'class A(B..):
    extract the total hierarchy "
  (interactive)
  (goto-char (point-min))
  (let (graph sorted-graph
        (regex "^class \\(.+?\\)\\((\\(.*?\\))\\)?:$")
        )
    (while (and (< (point) (point-max)) (looking-at regex))
      (let* ((classname (match-string 1))
             (parents   (s-split "," (if (match-string 3)
                                         (match-string 3)
                                       "") t))
             (cleaned-parents (mapcar #'(lambda (x) (s-trim (if (s-contains? "." x)
                                                           (cadr (s-split "\\." x t))
                                                         x))) parents)))

        (setq graph (concatenate 'list graph (if (null cleaned-parents)
                                                 `(("object" . ,classname))
                                               (-zip-fill classname cleaned-parents nil))))
        )
      (forward-line)
      )
    (setq sorted-graph (-sort #'(lambda (x y) (string-lessp (car x) (car y))) graph))
    ;; Generate Graphviz description
    (with-temp-buffer-window "*Python Class Diagram*"
        'display-buffer-pop-up-window nil
      (princ "#+begin_src dot :file ~/desktop/class-diagram.png :exports results :results silent\n")
      (princ "graph {\n")
      ;; Add individual elements
      (mapc #'(lambda (x) (princ (format "  \"%s\" -- \"%s\";\n" (car x) (cdr x)))) sorted-graph)
      (princ "}\n")
      (princ "#+end_src\n")
      (princ "[[file:~/desktop/class-diagram.png][Results]]\n")
      )
    )
  )

(defun +jg-python-summarize-goto (buff pos &rest args)
  (if-let ((wind (get-buffer-window buff)))
      (progn
        (select-window wind)
        (widen)
        (goto-char pos)
        (beginning-of-line)
        (recenter)
        )
    )
  )
(defun +jg-python-summarize ()
  (interactive)
  (let ((buffer (current-buffer))
        links)
    (with-current-buffer (current-buffer)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward python-nav-beginning-of-defun-regexp nil t)
            (push `(,(point) . ,(match-string 0)) links)
            )
          )
        )
      )
    (message "Extracted")
    (with-current-buffer (get-buffer-create jg-python-summary-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (cl-loop for link in (reverse links)
               do
               (insert-text-button (propertize (cdr link) 'read-only nil)
                                   'action (-partial #'+jg-python-summarize-goto
                                                     buffer (car link)))
               (insert "\n")
               )
      (read-only-mode 1)
      )
    (display-buffer (get-buffer jg-python-summary-buffer))
    )
  )

;;-- end summary

(defun +jg-python-swipe-to-def ()
  (interactive)
  ;; TODO make more robust
  ;; TODO process to mark class owner
  ;; TODO include vars / arg nums
  (message "Swiping to def: %s" default-directory)
  (let* ((current-file (buffer-file-name))
         ;; Get global's stored symbols, filtering for functions
         (result (with-temp-buffer
                   (list :exit-status
                         (shell-command
                          (format "global -f %s | grep -E \"\.py\s+(def|class) .+?\(.+?\)( -> .+?)?:(.+)?$\"" current-file)
                          (current-buffer))
                         :output
                         (split-string (buffer-string) "\n" t "\s+"))))
         ;; Process into an alist
         (processed (cl-loop for line in (plist-get result :output)
                             for components = (split-string line "\s+" t "\s+")
                             collect (cons (car components) (nth 1 components))))
         )
    ;; select and move
    (ivy-read "Select Function: " processed :action (lambda (x) (goto-line (string-to-number (cdr x)))))
    )
  )


(defun +jg-python-related-files-fn (path)
  " Provide projectile with various :kinds of related file "
  (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
        (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
        (init-file  (f-join (f-parent path) "__init__.py"))
        (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
        (error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
        (project    (f-join (projectile-project-root) "pyproject.toml"))
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

(defun +jg-python-outline-level ()
  (current-indentation)
  )
