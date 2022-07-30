;;; +hooks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 05, 2022
;; Modified: April 05, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/johngrey/+hooks
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

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

(define-advice +python-use-correct-flycheck-executables-h (:override ()
                                                           +jg-python-flycheck-override)
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

(defun +jg-python-auto-kill-anaconda-processes-h ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (progn (message "Killing Ananaconda")
             (anaconda-mode-stop))))

(defun +jg-python-auto-kill-conda-hook ()
  (add-hook 'kill-buffer-hook
            #'+jg-python-auto-kill-anaconda-processes-h
            nil 'local)
  )

(defun +jg-python-auto-hide ()
  " Add auto-hiding to python.
Hides imports in a vimish fold,
Add any sections commented with jg-python-fold-block-[start|end]-re
and closes classes and functions, re-opening only the first class "
  (message "Running Python Auto Hide")
  (if jg-text-auto-hide-toggle
      (save-excursion
        (beginning-of-buffer)
        (evil-open-folds)
        ;; Fold Imports
        (message "Searching for import block")
        (if (re-search-forward "^\\(from\\|import\\|##-- imports\\)" nil t)
            (let* ((start-hide (progn (beginning-of-line) (point)))
                   (end-hide (progn (forward-line)
                                    (re-search-forward jg-python-import-block-end-re nil t)
                                    (re-search-backward "^\\(from\\|import\\|##-- end imports\\)" (+ 10 start-hide) t)
                                    (end-of-line)
                                    (point)))
                   )
              (if (and start-hide end-hide (not (= start-hide end-hide)) (not (vimish-fold--folds-in start-hide end-hide)))
                  (vimish-fold start-hide end-hide)
                )
              )
          )
        (forward-line)
        ;; Semi-Fold everything
        (beginning-of-buffer)
        (evil-close-folds)
        (if (re-search-forward "^class " nil t)
            (let ((current-prefix-arg t))
              (evil-close-fold))
          )
        )
    )
  )

;;; +hooks.el ends here
