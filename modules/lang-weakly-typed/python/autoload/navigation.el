;;; navigation.el -*- lexical-binding: t; -*-


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
(defun +jg-python-outline-level ()
  (current-indentation)
  )

;;;###autoload
(defun +jg-python-breakpoint-line ()
  " Get current file path and line, for using in python debugger "
  (interactive)
  (kill-new (concat "b " (buffer-file-name) " : "  (format "%s" (line-number-at-pos))))
  )
