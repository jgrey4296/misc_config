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

;; Jump funcs
(defun +jg-python-browse-bibex-parser ()
  (interactive)
  (+jg-browse-url jg-python-bibtex-parser-url)
  )
(defun +jg-python-browse-pydocs ()
  (interactive)
  (+jg-browse-url jg-python-py-docs-url)
  )
(defun +jg-python-browse-beautiful-soup ()
  (interactive)
  (+jg-browse-url jg-python-beautiful-soup-url)
  )
