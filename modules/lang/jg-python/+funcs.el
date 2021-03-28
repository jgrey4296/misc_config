;;; lang/jg-python/+funcs.el -*- lexical-binding: t; -*-
(defun +jg-python-close-all-defs ()
    (interactive)
    (let* ((close #'outline-hide-subtree)
           )
      (save-excursion
        (goto-char (point-min))
        (while (python-nav-forward-defun)
          (funcall close)
          )
        )
      )
    )
(defun +jg-python-close-class-defs ()
    (interactive )
    (save-excursion
      (re-search-backward "^class ")
      (outline-show-subtree)
      (forward-line)
      (while (and (not (looking-at-p "^class "))
                  (python-nav-forward-defun))
        (beginning-of-line)
        (if (not (looking-at-p "^class "))
            (outline-hide-subtree)
          )
        (end-of-line)
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
          (back-to-indentation)
          (insert trace)
          (insert "\n")
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
