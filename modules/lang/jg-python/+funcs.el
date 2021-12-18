;;; lang/jg-python/+funcs.el -*- lexical-binding: t; -*-
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
(defun +jg-python-toggle-breakpoint ()
    "Modified version of spacemacs original
Add a break point, highlight it.
Customize python using PYTHONBREAKPOINT env variable
"
    (interactive)
    (let ((trace "breakpoint()")
          (line (thing-at-point 'line)))
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (evil-end-of-line)
          (insert "\n")
          (insert trace)
          (python-indent-line)))))

(defun +jg-python-insert-import (&optional arg)
  " insert the literal string provided/read from minibuffer, at the imports section
of a python file "
  (interactive)
  (let ((arg (if (not arg) (read-string "Import Statement: " "import ") arg)))
    (pyimport--insert-import arg))
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
;; Hooks
(defun +jg-python-outline-regexp-override-hook ()
  (setq-local outline-regexp
              (python-rx (or (: line-start (>= 2 eol))
                             (: line-start ?#)
                             (: line-start upper)
                             (: (* space) block-start)
                             (: (* space) ?@)
                             ))
              )
  )
(defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8")))
(defun +python-init-anaconda-mode-maybe-h ()
      "Enable `anaconda-mode' if `lsp-mode' is absent and
`python-shell-interpreter' is present."
      (unless (or (bound-and-true-p lsp-mode)
                  (bound-and-true-p eglot--managed-mode)
                  (bound-and-true-p lsp--buffer-deferred)
                  (not (executable-find python-shell-interpreter)))
        (anaconda-mode)))
(defun +python-auto-kill-anaconda-processes-h ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (anaconda-mode-stop)))
(defun +python-auto-kill-hook-fn ()
    (add-hook 'kill-buffer-hook #'+python-auto-kill-anaconda-processes-h
              nil 'local))
