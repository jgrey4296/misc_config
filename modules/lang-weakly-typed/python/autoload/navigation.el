;;; +nav.el -*- lexical-binding: t; -*-
;; Customisations of Conda navigation

;;;###autoload
(defun +jg-conda-pop-to-xref (result)
  (if (stringp result)
      (message result)
    (let* ((window-ring-suppress-adding t)
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

;;;###autoload
(defun +jg-python-select-defun ()
  (interactive)
  (let ((start (progn (python-nav-beginning-of-defun)
                      (point)))
        (end (progn (python-nav-end-of-defun)
                    (point))))
    (evil-visual-make-region start end))
  )

;;;###autoload
(defun +jg-python-select-class ()
  (interactive)
  (let ((start (re-search-backward "^class")))
    (python-nav-end-of-defun)
    (evil-visual-make-region start (point))
    )
  )

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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
                          (format "global -f %s | grep -E \"\.py\s+

(def|class) .+?\(.+?\)( -> .+?)?:(.+)?$\"" current-file)
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

;;;###autoload
(defun +jg-python-related-files-fn (path)
  " Provide projectile with various :kinds of related file "
  (let* ((root (projectile-project-root))
         (fbase   (f-base path))
         (fname   (f-filename path))
         (fparent (f-parent path))

         (doot-toml  (f-join root "doot.toml"))
         (tasks-file (f-join root "doot_tasks.toml"))
         (tasks-dir  (f-join root "doot_tasks"))
         (project    (f-join root "pyproject.toml"))
         (log-file   (f-join root (concat "log." fbase)))

         (impl-file  (f-join root (f-parent fparent) (s-replace "test_" "" fname)))
         (test-file  (f-join root fparent "__tests" (concat "test_" fname)))
         (init-file  (f-join root fparent "__init__.py"))
         (error-file (f-join root (car (f-split path)) "errors" (concat fbase "_errors.py")))

         (is-test      (s-matches? "^test_" fname))
         (is-dooter    (s-matches? "dooter.py" fname))
         (is-doot-toml (s-matches? "doot.toml" fname))
         )
    (append (when (and is-test (f-exists? impl-file))
              (list :impl impl-file))
            (when (and (not is-test) (f-exists? test-file))
              (list :test test-file))

            (when (f-exists? doot-toml)
              (list :config doot-toml))
            (when (f-exists? tasks-file)
              (list :tasks tasks-file))
            (when (f-exists? tasks-dir)
              (list :tasks tasks-dir))

            (when (f-exists? project)
              (list :pyproject project))
            (when (f-exists? init-file)
              (list :init-py init-file))
            (when (f-exists? log-file)
              (list :log log-file))
            (when (f-exists? error-file)
              (list :errors error-file))
            )
    )
  )

;;;###autoload
(defun +jg-python-outline-level ()
  (current-indentation)
  )

;;;###autoload
(defun +jg-python-breakpoint-line ()
  " Get current file path and line, for using in python debugger "
  (interactive)
  (kill-new (concat "b " (buffer-file-name) " : "  (format "%s" (line-number-at-pos))))
  )
